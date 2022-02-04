//! Allocate memory at compile time!
//!
//! Currently, in stable rust there is no way to dynamically allocate or deallocate memory at compile time (i.e. in `const fn`s).
//! This crate allows you to do exactly that, in nightly rust, with the help of a few intrinsics and a lot of unstable features, so
//! don't be surprised if it suddenly breaks, and _please_ don't use it in production yet.
//!
//! The crate exposes one type, [`ConstAlloc`], which wraps any allocator and itself implements [`const Allocator`]. Using this type
//! you can allocate and deallocate memory in `const fn`s, which would theoretically also allow you to use something like [`Box<T>`]
//! or [`Vec<T>`] in `const` contexts. Unfortunately, none of the relevant member functions on those types are `const` yet though, so
//! unfortunately it's still not really possible (yet?) to use any standard library collection at compile time.
//!
//! [`const Allocator`]: alloc::alloc::Allocator
//! [`Box<T>`]: alloc::boxed::Box
//! [`Vec<T>`]: alloc::vec::Vec

#![feature(
    allocator_api,
    const_eval_select,
    const_fn_trait_bound,
    const_heap,
    const_intrinsic_copy,
    const_nonnull_new,
    const_option_ext,
    const_ptr_is_null,
    const_ptr_offset,
    const_ptr_write,
    const_slice_from_raw_parts,
    const_trait_impl,
    core_intrinsics
)]
#![cfg_attr(not(test), no_std)]

extern crate alloc;

/// A `const` compatible wrapper over any Allocator type.
///
/// The wrapped Allocator doesn't have to implement [`const Allocator`], only [`Allocator`].
/// This wrapper will forward any calls to [`allocate`] at compile time to the [`const_allocate`] intrinsic.
/// Likewise it will forward [`deallocate`] calls at compile time to the [`const_deallocate`] intrinsic.
///
/// [`const Allocator`]: alloc::alloc::Allocator
/// [`allocate`]: [`ConstAlloc::allocate`]
/// [`deallocate`]: [`ConstAlloc::deallocate`]
/// 
/// # Examples
///
/// ```rust
/// const fn alloc_and_dealloc<A: ~const Allocator>(a: &A) {
///     let ptr = a.allocate(Layout::new::<[u8; 128]>());
///     if let Ok(ptr) = ptr {
///         unsafe { a.deallocate(ptr.cast(), Layout::new::<[u8; 128]>()) };
///     }
/// }
///
/// static ALLOC_COUNT: AtomicUsize = AtomicUsize::new(0);
/// static DEALLOC_COUNT: AtomicUsize = AtomicUsize::new(0);
/// 
/// struct CountingAlloc;
/// unsafe impl Allocator for CountingAlloc {
///     fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
///         ALLOC_COUNT.fetch_add(1, Ordering::SeqCst);
///         System.allocate(layout)
///     }
/// 
///     unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
///         DEALLOC_COUNT.fetch_add(1, Ordering::SeqCst);
///         System.deallocate(ptr, layout)
///     }
/// }
/// static A: ConstAlloc<CountingAlloc> = ConstAlloc(CountingAlloc);
/// #[used]
/// static CT: () = alloc_and_dealloc(&A);
/// alloc_and_dealloc(&A);
///
/// // CountingAlloc::allocate will only be called during the runtime call to alloc_and_dealloc
/// assert_eq!(ALLOC_COUNT.load(Ordering::SeqCst), 1);
/// assert_eq!(DEALLOC_COUNT.load(Ordering::SeqCst), 1);
/// ```
pub struct ConstAlloc<A>(A);

use alloc::alloc::{AllocError, Allocator, Layout};
use core::intrinsics::{const_allocate, const_deallocate, const_eval_select};
use core::ptr::{copy_nonoverlapping, slice_from_raw_parts_mut, write_bytes, NonNull};

type AllocResult = Result<NonNull<[u8]>, AllocError>;

const fn to_alloc_result(ptr: *mut u8, size: usize) -> AllocResult {
    NonNull::new(slice_from_raw_parts_mut(ptr, size)).ok_or(AllocError)
}

const fn alloc<A: Allocator>(a: &A, layout: Layout) -> AllocResult {
    const fn ct<A: Allocator>(_: &A, layout: Layout) -> AllocResult {
        unsafe { to_alloc_result(const_allocate(layout.size(), layout.align()), layout.size()) }
    }
    unsafe { const_eval_select((a, layout), ct, A::allocate) }
}

const unsafe fn dealloc<A: Allocator>(a: &A, ptr: NonNull<u8>, layout: Layout) {
    fn rt<A: Allocator>(a: &A, ptr: NonNull<u8>, layout: Layout) {
        unsafe { a.deallocate(ptr, layout) };
    }

    const fn ct<A: Allocator>(_: &A, ptr: NonNull<u8>, layout: Layout) {
        unsafe { const_deallocate(ptr.as_ptr(), layout.size(), layout.align()) };
    }
    const_eval_select((a, ptr, layout), ct, rt)
}

const fn alloc_zeroed<A: Allocator>(a: &A, layout: Layout) -> AllocResult {
    const fn ct<A: Allocator>(_: &A, layout: Layout) -> AllocResult {
        unsafe {
            let ptr = const_allocate(layout.size(), layout.align());
            if !ptr.is_null() {
                write_bytes(ptr, 0, layout.size());
            }
            to_alloc_result(ptr, layout.size())
        }
    }

    unsafe { const_eval_select((a, layout), ct, A::allocate_zeroed) }
}

const unsafe fn grow<A: Allocator>(
    a: &A,
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
) -> AllocResult {
    fn rt<A: Allocator>(
        a: &A,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> AllocResult {
        unsafe { a.grow(ptr, old_layout, new_layout) }
    }

    const fn ct<A: Allocator>(
        _: &A,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> AllocResult {
        unsafe {
            let new_ptr = const_allocate(new_layout.size(), new_layout.align());
            if !new_ptr.is_null() {
                copy_nonoverlapping(ptr.as_ptr(), new_ptr, old_layout.size());
            }
            const_deallocate(ptr.as_ptr(), old_layout.size(), old_layout.align());
            to_alloc_result(new_ptr, new_layout.size())
        }
    }

    const_eval_select((a, ptr, old_layout, new_layout), ct, rt)
}

const unsafe fn grow_zeroed<A: Allocator>(
    a: &A,
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
) -> AllocResult {
    fn rt<A: Allocator>(
        a: &A,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> AllocResult {
        unsafe { a.grow_zeroed(ptr, old_layout, new_layout) }
    }

    const fn ct<A: Allocator>(
        _: &A,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> AllocResult {
        unsafe {
            let new_ptr = const_allocate(new_layout.size(), new_layout.align());
            if !new_ptr.is_null() {
                copy_nonoverlapping(ptr.as_ptr(), new_ptr, old_layout.size());
                write_bytes(
                    ptr.as_ptr().add(old_layout.size()),
                    0,
                    new_layout.size() - old_layout.size(),
                );
            }
            const_deallocate(ptr.as_ptr(), old_layout.size(), old_layout.align());
            to_alloc_result(new_ptr, new_layout.size())
        }
    }

    const_eval_select((a, ptr, old_layout, new_layout), ct, rt)
}

const unsafe fn shrink<A: Allocator>(
    a: &A,
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
) -> AllocResult {
    fn rt<A: Allocator>(
        a: &A,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> AllocResult {
        unsafe { a.shrink(ptr, old_layout, new_layout) }
    }

    const fn ct<A: Allocator>(
        _: &A,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> AllocResult {
        unsafe {
            let new_ptr = const_allocate(new_layout.size(), new_layout.align());
            if !new_ptr.is_null() {
                copy_nonoverlapping(ptr.as_ptr(), new_ptr, new_layout.size());
            }
            const_deallocate(ptr.as_ptr(), old_layout.size(), old_layout.align());
            to_alloc_result(new_ptr, new_layout.size())
        }
    }

    const_eval_select((a, ptr, old_layout, new_layout), ct, rt)
}

unsafe impl<A: Allocator> const Allocator for ConstAlloc<A> {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        alloc(&self.0, layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        dealloc(&self.0, ptr, layout);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        alloc_zeroed(&self.0, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        grow(&self.0, ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        grow_zeroed(&self.0, ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        shrink(&self.0, ptr, old_layout, new_layout)
    }
    fn by_ref(&self) -> &Self {
        self
    }
}

#[cfg(test)]
mod tests {
    use std::{
        alloc::System,
        sync::atomic::{AtomicUsize, Ordering},
    };

    use super::*;

    #[test]
    fn test_name() {
        const fn alloc_and_dealloc<A: ~const Allocator>(a: &A) {
            let ptr = a.allocate(Layout::new::<[u8; 128]>());
            if let Ok(ptr) = ptr {
                unsafe { a.deallocate(ptr.cast(), Layout::new::<[u8; 128]>()) };
            }
        }

        static ALLOC_COUNT: AtomicUsize = AtomicUsize::new(0);
        static DEALLOC_COUNT: AtomicUsize = AtomicUsize::new(0);
        struct CountingAlloc;
        unsafe impl Allocator for CountingAlloc {
            fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
                ALLOC_COUNT.fetch_add(1, Ordering::SeqCst);
                System.allocate(layout)
            }

            unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
                DEALLOC_COUNT.fetch_add(1, Ordering::SeqCst);
                System.deallocate(ptr, layout)
            }
        }
        static A: ConstAlloc<CountingAlloc> = ConstAlloc(CountingAlloc);
        #[used]
        static CT: () = alloc_and_dealloc(&A);
        alloc_and_dealloc(&A);

        // CountingAlloc::allocate will only be called during the runtime call to alloc_and_dealloc
        assert_eq!(ALLOC_COUNT.load(Ordering::SeqCst), 1);
        assert_eq!(DEALLOC_COUNT.load(Ordering::SeqCst), 1);
    }
}
