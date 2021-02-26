--- Compute the n-th fibonacci number.
function fib(n)
  local a, b = 0, 1
  for i = 0, (n - 1) do
    a, b = b, b + a
  end
  return a
end
