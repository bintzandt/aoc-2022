List<int> getParsedInput() {
    string[] input = File.ReadAllLines("input.txt");
    List<int> parsedInput = new List<int>();
    int sum = 0;
    foreach (var value in input) {
        if (value == "") {
            parsedInput.Add(sum);
            sum = 0;
        } else {
            sum += int.Parse(value);
        }
    }
    return parsedInput;
}

var input = getParsedInput();

Console.WriteLine("Task 1: " + input.Max());

// Sort the List
input.Sort();
// Reverse it
input.Reverse();
Console.WriteLine("Task 2: " + input.Take(3).Sum());