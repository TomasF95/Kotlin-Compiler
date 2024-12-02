fun main() {
    // Declare and initialize variables
    val price: Int = (150 + 20 * 12);
    var quantity: Int = 10;
    var discount: Double = 0.0;

    // Print initial values
    print(price);
    print(quantity);
    print(discount);

    // Arithmetic operation to calculate discount
    discount = (price * quantity) * 0.1;
    print(discount);

    // While loop
    var attempts: Int = 3;
    while (attempts > 0) {
        print("Attempts remaining: ");
        print(attempts);
        attempts = attempts - 1;
    }

    // If statement
    val isAvailable: Boolean = true;
    val isExpired: Boolean = false;
    if (isExpired) {print("Expired");}

    // If-else statement
    if (quantity >= 10) {
        print("Enough quantity available");
    } else {
        print("Insufficient quantity");
    }

    // Increment and Decrement
    var count: Int = 5;
    count++;
    count /= 2;

    // Logical precedence ( && > || )
    val p: Boolean = true;
    val q: Boolean = false;
    if (p && q || p) {}
    if (q || p && p) {}
    if (isAvailable || isExpired && p || q) {}
    if (p && isAvailable || q && isExpired) {}
}
