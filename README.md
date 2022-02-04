# const-alloc

[Link to the docs!](https://docs.rs/const-alloc)

Allocate memory at compile time!

Currently, in stable rust there is no way to dynamically allocate or deallocate memory at compile time (i.e. in `const fn`s).
This crate allows you to do exactly that, in nightly rust, with the help of a few intrinsics and a lot of unstable features, so
don't be surprised if it suddenly breaks, and _please_ don't use it in production yet.

The crate exposes one type, `ConstAlloc`, which wraps any allocator and itself implements `const Allocator`. Using this type
you can allocate and deallocate memory in `const fn`s, which would theoretically also allow you to use something like `Box<T>`
or `Vec<T>` in `const` contexts. Unfortunately, none of the relevant member functions on those types are `const` yet though, so
unfortunately it's still not really possible (yet?) to use any standard library collection at compile time.

License: Apache-2.0 OR MIT
