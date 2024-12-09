fun main() {
    // Variable Declarations and Assignments
    val calc: Int = (5 + 9 * 7);
    var life: Int = 42;
    var result: Double = 0.0;

    // print initial values
    print(calc);
    print(life);
    print(result);

    // arithmetic expressions
    result = (calc + life) / 2.0;
    print(result);

    // while loop
    var counter: Int = 3;
    while (counter > 0) {
        print(counter);
        counter = counter - 1;
    }

    // if statement
    val a: Boolean = true;
    val b: Boolean = false;
    if (!a) {print(0);}

    // if else statement
    if (life > 40) {
        print("Life is greater than 40");
    } else {
        print("Life is not greater than 40");
    }

    // increment and decrement
    var x: Int = 0;
    x++;
    x*=2;

    // logical precedence ( && > || )
    val c: Boolean = true;
    val d: Boolean = false;
    if (c && d || c) {}
    if (c || d && c) {}
    if (a || b && c || d) {}
    if (a && b || c && d) {}
}
