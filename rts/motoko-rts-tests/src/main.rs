#![feature(ptr_offset_from, map_first_last)]

mod bigint;
mod closure_table;
mod crc32;
mod gc;
mod leb128;
mod principal_id;
mod text;
mod utf8;

use motoko_rts::debug;
use motoko_rts::gc_common::collect_internal;
use motoko_rts::types::*;

#[macro_use]
extern crate maplit;

fn main() {
    if std::mem::size_of::<usize>() != 4 {
        println!("Motoko RTS only works on 32-bit architectures");
        std::process::exit(1);
    }

    let refs = &btreemap! {
        0 => vec![0, 2],
        2 => vec![0],
        3 => vec![3],
    };

    let heap_1 = gc::MotokoHeap::new(&refs, &[0, 2, 3]);

    println!("{:?}", heap_1.heap);

    unsafe {
        debug::dump_heap(
            // get_heap_base
            || heap_1.heap_base_address() as u32,
            // get_hp
            || heap_1.heap_ptr_address() as u32,
            // get_static_roots
            || skew(heap_1.static_root_array_address()),
            // get_closure_table_loc
            || heap_1.closure_table_address() as *mut SkewedPtr,
        );
    }

    for _ in 0..3 {
        unsafe {
            collect_internal(
                // get_heap_base
                || heap_1.heap_base_address() as u32,
                // get_hp
                || heap_1.heap_ptr_address() as u32,
                // set_hp
                |_hp| {},
                // note_live_size
                |_live_size| {},
                // note_reclaimed
                |_reclaimed| {},
                // get_static_roots
                || skew(heap_1.static_root_array_address()),
                // get_closure_table_loc
                || heap_1.closure_table_address() as *mut SkewedPtr,
                // grow_memory
                |ptr| {
                    if ptr - (heap_1.heap.as_ptr() as usize) > heap_1.heap.len() {
                        panic!(
                            "grow_memory ptr={:#x}, heap size={:#x}",
                            ptr,
                            heap_1.heap.len()
                        )
                    }
                },
            );
        }
    }

    // unsafe {
    //     closure_table::test();
    //     bigint::test();
    //     utf8::test();
    //     crc32::test();
    //     principal_id::test();
    //     text::test();
    //     leb128::test();
    // }
}

unsafe fn as_u8_slice(v: &[u32]) -> &[u8] {
    std::slice::from_raw_parts(
        v.as_ptr() as *const u8,
        v.len() * std::mem::size_of::<u32>(),
    )
}

// Called by the RTS to panic
#[no_mangle]
extern "C" fn rts_trap(ptr: *const u8, len: Bytes<u32>) -> ! {
    let msg = unsafe { std::slice::from_raw_parts(ptr, len.0 as usize) };
    match core::str::from_utf8(msg) {
        Err(err) => panic!(
            "rts_trap_with called with non-UTF8 string (error={:?}, string={:?})",
            err, msg
        ),
        Ok(str) => panic!("rts_trap_with: {:?}", str),
    }
}

// Called by RTS BigInt functions to panic. Normally generated by the compiler
#[no_mangle]
extern "C" fn bigint_trap() -> ! {
    panic!("bigint_trap called");
}

// Called by the RTS for debug prints
#[no_mangle]
unsafe extern "C" fn print_ptr(ptr: usize, len: u32) {
    let str: &[u8] = core::slice::from_raw_parts(ptr as *const u8, len as usize);
    println!("[RTS] {}", String::from_utf8_lossy(str));
}
