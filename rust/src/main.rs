
const TEST_INPUT_1: &str = "80871224585914546619083218645595";
const INPUT: &str = "59713137269801099632654181286233935219811755500455380934770765569131734596763695509279561685788856471420060118738307712184666979727705799202164390635688439701763288535574113283975613430058332890215685102656193056939765590473237031584326028162831872694742473094498692690926378560215065112055277042957192884484736885085776095601258138827407479864966595805684283736114104361200511149403415264005242802552220930514486188661282691447267079869746222193563352374541269431531666903127492467446100184447658357579189070698707540721959527692466414290626633017164810627099243281653139996025661993610763947987942741831185002756364249992028050315704531567916821944";
const INPUT_LEN: usize = INPUT.len();
const REPEAT: usize = 10;
const NUM_LEN: usize = INPUT_LEN * REPEAT;


fn parse_input() -> [i32; INPUT_LEN] {
    let mut numbers: [i32; INPUT_LEN] = [0; INPUT_LEN];
    let mut chars = INPUT.chars();

//    println!("{}", chars);

    for i in 0..INPUT_LEN {
        let c = chars.next();
//        println!("/ {:?}", c);
        let d = c.unwrap().to_digit(10);
//        println!("{:?}", d);
        numbers[i] = d.unwrap() as i32;
    }

    return numbers;
}

fn repeat_input(numbers: [i32; INPUT_LEN]) -> Box<[i32; NUM_LEN]> {
    let mut effective_numbers = Box::new ([0; NUM_LEN]);
    
    for i in 00..NUM_LEN {
        effective_numbers[i] = numbers[i % INPUT_LEN];
    }
    
    return effective_numbers;
}

fn main() {
    println!("hallo");
    let numbers = parse_input();
    let effective_numbers: Box<[i32; NUM_LEN]> = repeat_input(numbers);

    let nth_phase = n_phases(100, effective_numbers);

    for i in 0..nth_phase.len() {
        print!("{}", nth_phase[i]);
    }
}

fn n_phases(n: u16, mut numbers: Box<[i32; NUM_LEN]>) -> Box<[i32; NUM_LEN]> {
    for i in 0..n {
        numbers = next_phase(numbers);
    }

    return numbers;
}

const BASE_PATTERN: [i32; 4] = [0, 1, 0, -1];

fn base_number_at(position: usize, nth_round: usize) -> i32 {
    let pos = ((position + 1) / nth_round) % 4;
    return BASE_PATTERN[pos];
}

fn next_phase(numbers: Box<[i32; NUM_LEN]>) -> Box<[i32; NUM_LEN]> {
    let mut next_phase: Box<[i32; NUM_LEN]> = Box::new([0; NUM_LEN]);

    for i in 0..numbers.len() {
        let mut n: i32 = 0;

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
