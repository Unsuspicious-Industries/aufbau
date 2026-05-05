function fib(n: number): number {
  if (n === 0) 0 else if (n === 1) 1 else fib(n - 1) + fib(n - 2)
}
let result: number = fib(10);
result;
