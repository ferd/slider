require 'benchmark'
n = 50000
base_a = []
base_s = Set.new

for i in 1..n
    r = rand(1..30000)
    base_a << r
    base_s << r
end

Benchmark.bm(7) do |x|
    x.report("set append") { s = Set.new; for i in 1..n; s << i; end }
    x.report("array append") { a = []; for i in 1..n; a << i; end }
    x.report("set exist?") { for i in 1..30000; base_s.include?(i); end }
    x.report("array exist?") { for i in 1..30000; base_a.include?(i); end }
end
