fn main() {
    cc::Build::new()
        //.file("../EX1/iterative.c")
        .file("../EX1/recursive.c")
        //.compile("iter");
        .compile("recu");
}