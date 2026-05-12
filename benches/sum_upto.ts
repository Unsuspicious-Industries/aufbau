// Compute sum of numbers from 1 to n.

function sum_upto(n: number): number {
  let s: number = 0;
  while (n > 0) {
    s = s + n;
    n = n - 1;
  }
  s
}

let result: number = sum_upto(5);
result;
