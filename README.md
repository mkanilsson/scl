# scl (Shitty C-like language)

A simple c like language that is pretty shit. Compiles to QBE.

> This is a for-fun project for me to learn some compile stuff

## Examples

Checkout [src/tests/sources/runtime](src/tests/sources/runtime) for how to use the language.

## Syntax and semantics

This is a program that I would like to see run successfully some day :D

```scl
@link_name("malloc")
extern proc libc_malloc<T>(u32) ?owned T

proc malloc<T>() ?owned T {
    libc_malloc::<T>(@size_of::<T>())
}

extern proc free<T>(owned T) void

union Result<TOk, TErr> {
    Ok(TOk)
    Err(TErr)
}

enum AllocatorError {
    OutOfMemory = 1
}

trait Allocator {
    proc allocate<T>(this) Result<owned T, AllocatorError>
    proc deallocate<T>(this, ptr: owned T) void


    proc allocate_slice<T>(this, count: usize) Result<&[T], AllocatorError>
    proc deallocate_slice<T>(this, s: &[T]) void
}

struct LibCAllocator;

impl LibCAllocator {
    proc malloc<T>(count: usize) Result<owned T, AllocatorError> {
        if (libc_malloc::<T>(@size_of::<T>() * count)) |ptr| {
            ::Ok(ptr)
        } else {
            ::Err(::OutOfMemory)
        }
    }
}

impl Allocator for LibCAllocator {
    proc allocate<T>(this) Result<owned T, AllocatorError> {
        this.malloc::<T>(count) 
    }

    proc deallocate<T>(ptr: owned T) void {
        free(ptr);
    }


    proc allocate_slice<T>(count: usize) Result<&[T], AllocatorError> {
        slice::from_raw(this.malloc::<T>(count)?, count)
    }

    proc deallocate_slice<T>(s: &[T]) void {
        free(s.ptr());
    }
}

struct Thing {
    v: i32
}

proc alloc_thing(allocator: virtual Allocator, value: i32) owned T {
    switch (allocator.allocate::<Thing>) {
        Ok |ptr| => {
            ptr.*.v = value;
            ptr
        }
        Err => {
            @panic("Couldn't allocate")
        }
    }
}

proc main() void {
    let libc_allocator = LibCAllocator {};

    let t = alloc_thing(libc_allocator, 123);
    defer libc_allocator.deallocate(t);

    printf("t.v = %d\n", t.*.v);
}
```
