object MyObject extends MyTrait {
    def func = new MyClass
    def f(x: Int, y: Int) = x + y
    def g(x: Int) = f(f(1, 2), f(3, 4))
    lazy val a = func.z
}

