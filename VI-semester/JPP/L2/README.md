# Programming Languages and Paradigms

## Task 1
Let's consider a finite field with characteristic 1234577 (division is defined as $`a / b = a \cdot b^{-1}`$, where $`b^{-1}`$ has the property $`b \cdot b^{-1} = 1\:mod\:1234577`$ and can be computed by solving the Diophantine equation).

Create a class in C++ (conforming to at least the C++14 standard) that implements the finite field GF1234577. Ensure that the class:

- Overloads all useful comparison operators (`==`, `!=`, `<=`, `>=`, `<`, `>`), arithmetic operations (`+`, `-`, `*`, `/`), and assignment operations (`=`, `+=`, `-=`, `*=`, `/=`).
- Has a set of appropriate constructors, useful conversions, and stream operator handling.
- Includes a method that returns the characteristic of the class.
- Uses the `const` keyword where applicable for method arguments to make them immutable within the method and protect them from implementation errors.
- Handles error cases with exceptions.
- Ensures that the public section contains only essential methods.
- Is designed to be easily adaptable to a finite field with a different characteristic.

Write a program to test the implemented class.

## Task 2
Create an analogous class in Java as in Task 1 (note that we cannot overload operators here and must use methods) and an appropriate test class.

## Task 3
Repeat Task 1 in another programming language that follows the object-oriented programming paradigm. Ensure to fully utilize the capabilities of the chosen language.
