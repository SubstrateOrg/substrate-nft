use codec::{Compact, Encode};

fn assert_encode<T: Encode>(t: T, bytes: &[u8]) {
    let data = Encode::encode(&t);
    assert_eq!(data, bytes);
}

#[derive(Encode)]
enum TestEnum {
    A,
    B,
    C = 10,
}

#[derive(Encode)]
struct TestStruct {
    a: TestEnum,
    b: u32,
    c: TestEnum,
}

#[derive(Encode)]
enum TestEnum2 {
    A(TestEnum, u32, TestEnum),
    B(TestStruct),
}

#[test]
fn test_codec() {
    assert_encode(1u32, b"\x01\0\0\0");
    assert_encode(1u64, b"\x01\0\0\0\0\0\0\0");

    assert_encode(true, b"\x01");
    assert_encode(false, b"\x00");

    assert_encode(TestEnum::A, b"\x00");
    assert_encode(TestEnum::B, b"\x01");
    assert_encode(TestEnum::C, b"\x0a");

    assert_encode((1u32, 2u32), b"\x01\0\0\0\x02\0\0\0");
    assert_encode((TestEnum::A, 2u32, TestEnum::C), b"\0\x02\0\0\0\x0a");

    assert_encode(
        TestStruct {
            a: TestEnum::A,
            b: 2u32,
            c: TestEnum::C,
        },
        b"\0\x02\0\0\0\x0a",
    );

    assert_encode(
        TestEnum2::A(TestEnum::A, 2u32, TestEnum::C),
        b"\0\0\x02\0\0\0\x0a",
    );
    assert_encode(
        TestEnum2::B(TestStruct {
            a: TestEnum::A,
            b: 2u32,
            c: TestEnum::C,
        }),
        b"\x01\0\x02\0\0\0\x0a",
    );

    assert_encode(Vec::<u8>::new(), b"\0");
    assert_encode(vec![1u32, 2u32], b"\x08\x01\0\0\0\x02\0\0\0");
}

// compact encoding:
// 0b00 00 00 00 / 00 00 00 00 / 00 00 00 00 / 00 00 00 00
//   xx xx xx 00															(0 .. 2**6)		(u8)
//   yL yL yL 01 / yH yH yH yL												(2**6 .. 2**14)	(u8, u16)  low LH high
//   zL zL zL 10 / zM zM zM zL / zM zM zM zM / zH zH zH zM					(2**14 .. 2**30)	(u16, u32)  low LMMH high
//   nn nn nn 11 [ / zz zz zz zz ]{4 + n}									(2**30 .. 2**536)	(u32, u64, u128, U256, U512, U520) straight LE-encoded

// Note: we use *LOW BITS* of the LSB in LE encoding to encode the 2 bit key.

#[test]
fn test_compact() {
    assert_encode(Compact(1u8), &[0b00000100]);
    assert_encode(Compact(1u64), &[0b00000100]);
    assert_encode(Compact(1u128), &[0b00000100]);

    assert_encode(Compact(63u8), &[0b11111100]);
    assert_encode(Compact(63u64), &[0b11111100]);
    assert_encode(Compact(63u128), &[0b11111100]);

    // 64
    assert_encode(Compact(0b01000000u8), &[0b00000001, 0b00000001]);
    assert_encode(Compact(0b01000000u64), &[0b00000001, 0b00000001]);
    assert_encode(Compact(0b01000000u128), &[0b00000001, 0b00000001]);

    // 65
    assert_encode(Compact(0b01000001u8), &[0b00000101, 0b00000001]);
    assert_encode(Compact(0b01000001u64), &[0b00000101, 0b00000001]);
    assert_encode(Compact(0b01000001u128), &[0b00000101, 0b00000001]);

    // 16383
    assert_encode(Compact((1u64 << 14) - 1), &[0b11111101, 0b11111111]);
    assert_encode(Compact((1u64 << 14) - 1), &[0b11111101, 0b11111111]);

    // 16384
    assert_encode(Compact(1u64 << 14), &[0b00000010, 0b00000000, 0b00000001, 0b00000000]);
    assert_encode(Compact(1u64 << 14), &[0b00000010, 0b00000000, 0b00000001, 0b00000000]);
}