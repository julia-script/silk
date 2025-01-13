// 1. Define a custom error type to represent possible errors.
#[derive(Debug)]
enum MyError {
    SomeError,
    CaughtError,
}

// 2. `foo` returns an error directly.
//    In Rust, we typically use a Result<T, E> for fallible functions.
fn foo() -> Result<(), MyError> {
    Err(MyError::SomeError)
}

// 3. `bar` propagates any error from `foo` using the `?` operator.
fn bar() -> Result<(), MyError> {
    foo()?;  // If `foo()` returns an error, `bar()` returns it immediately.

    Ok(())
}

// 4. `main` tries to run `bar()`. If `bar()` errors out, we "catch" it
//    by mapping the error to `MyError::CaughtError`.
fn main() -> Result<(), MyError> {
    match bar() {
        Ok(_) => {
            // Successfully ran `bar` with no error
            println!("bar() completed successfully");
            Ok(())
        }
        Err(_err) => {
            // bar() returned an error, we return `CaughtError` instead
            println!("Caught an error in bar()");
            Err(MyError::CaughtError)
        }
    }
}