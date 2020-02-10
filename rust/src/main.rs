use std::ptr::null;

const TEST_INPUT_1: &str = "80871224585914546619083218645595";
const EFFECTIVE_INPUT: &str = "59713137269801099632654181286233935219811755500455380934770765569131734596763695509279561685788856471420060118738307712184666979727705799202164390635688439701763288535574113283975613430058332890215685102656193056939765590473237031584326028162831872694742473094498692690926378560215065112055277042957192884484736885085776095601258138827407479864966595805684283736114104361200511149403415264005242802552220930514486188661282691447267079869746222193563352374541269431531666903127492467446100184447658357579189070698707540721959527692466414290626633017164810627099243281653139996025661993610763947987942741831185002756364249992028050315704531567916821944";

fn parse_input(input: &str) -> Vec<i16> {
    return input.chars().map(|c| c.to_digit(10).unwrap() as i16).collect();
}

fn main() {
    let numbers: Vec<i16> = vec![1, 2, 3, 4, 5, 6, 7, 8];
//    println!("{:?}", next_phase(numbers));

    println!("{:?}", n_phases(100, parse_input(EFFECTIVE_INPUT)));
}

fn n_phases(n: u16, mut numbers: Vec<i16>) -> Vec<i16> {
    for i in 0..n {
        numbers = next_phase(numbers);
    }

    return numbers;
}

const BASE_PATTERN: [i16; 4] = [0, 1, 0, -1];

fn base_number_at(position: usize, nth_round: usize) -> i16 {
    let pos = ((position + 1) / nth_round) % 4;
    return BASE_PATTERN[pos];
}

fn next_phase(numbers: Vec<i16>) -> Vec<i16> {
    let mut next_phase: Vec<i16> = vec![0; numbers.len()];

    for i in 0..numbers.len() {
        let mut n: i16 = 0;

        for j in 0..numbers.len() {
            let base_number = base_number_at(j, (i + 1));
            let number = numbers[j];
            n += number * base_number;
//            print!("{}*{} + ", number, base_number)
        }
        let effective_number = n.abs() % 10;
//        println!(" = {}", effective_number);
        next_phase[i] = effective_number;
    }

    return next_phase;
}

fn playing_around() {
    const INPUT: &str = "123456789";
    let base_pattern_lengthened = BASE_PATTERN.repeat(3);

    let chars = INPUT.chars();
    let numbers: Vec<u8> = chars.map(|c| c.to_digit(10).unwrap() as u8).collect();

    println!("{:?}", numbers);
    println!("{:?}", base_pattern_lengthened);
    let together: Vec<_> =
        base_pattern_lengthened.iter().zip(numbers.iter()).collect();
    println!("{:?}", together);

//    try_zip();


    let n: i32 = -1;
    let m: u8 = n.abs() as u8;
    println!("{} | {}", n, m);

    println!("{}", (2 << 15));
}

fn test_base_number_at() {
    println!("expected: -1 - is: {}", base_number_at(2, 1));
    println!("expected:  0 - is: {}", base_number_at(5, 1));
    println!("expected:  1 - is: {}", base_number_at(2, 2));
    println!("expected: -1 - is: {}", base_number_at(5, 2));
    println!("expected:  0 - is: {}", base_number_at(2, 5));
    println!("expected:  0 - is: {}", base_number_at(3, 5));
    println!("expected:  1 - is: {}", base_number_at(4, 5));
    println!("expected:  1 - is: {}", base_number_at(7, 5));
}
