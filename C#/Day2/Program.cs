string[] getParsedInput() {
    return File.ReadAllLines("input.txt");
}

int task1() {
   return getParsedInput().Select(row => row switch {
    "A X" => 4,
    "A Y" => 8,
    "A Z" => 3,
    "B X" => 1,
    "B Y" => 5,
    "B Z" => 9,
    "C X" => 7,
    "C Y" => 2,
    "C Z" => 6,
    _ => 0,
   }).Sum();
}

int task2() {
    return getParsedInput().Select(row => row switch {
    "A X" => 3,
    "A Y" => 4,
    "A Z" => 8,
    "B X" => 1,
    "B Y" => 5,
    "B Z" => 9,
    "C X" => 2,
    "C Y" => 6,
    "C Z" => 7,
    _ => 0,
   }).Sum();
}

Console.WriteLine("Task 1: " + task1());
Console.WriteLine("Task 2: " + task2());