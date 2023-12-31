// disable rust standard library
#![no_std]
// disables Rust runtime init,
#![no_main]
// see https://docs.rust-embedded.org/embedonomicon/smallest-no-std.html
#![feature(lang_items)]

#[lang = "eh_personality"]
extern "C" fn eh_personality() {}

use core::panic::PanicInfo;
use core::sync::atomic;
use core::sync::atomic::Ordering;

#[no_mangle]
/// The name **must be** `_start`, otherwise the compiler throws away all code as unused.
/// The name can be changed by passing a different entry symbol as linker argument.
fn _start() -> ! {
    let x = 12;
    let y = 21;
    let mut z = x + y;
    z = z / 3;

    struct string<'a> {
        capacity: u32,
        chars: &'a [char],
    }

    loop {}
}

#[inline(never)]
#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {
        atomic::compiler_fence(Ordering::SeqCst);
    }
}
