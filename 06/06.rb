#!/usr/bin/env ruby

def parse_races(inp)
  time, dist = inp.split("\n").map {|l| l.split(' ')[1..-1].map(&:to_i) }
  time.zip dist
end

def part_one(races)
  races.map do |time, dist|
    # Slow
    #(0..time).count {|t| (time - t) * t > dist }

    # Medium
    #start = (0..time).to_a.index  {|t| (time - t) * t > dist }
    #fin   = (0..time).to_a.rindex {|t| (time - t) * t > dist }
    #fin - start + 1

    # Fast
    times = (0..time).to_a
    first, second = times[0..times.size / 2], times[times.size / 2..-1]

    start = first.bsearch {|t| (time - t) * t > dist }
    fin   = second.bsearch {|t| (time - t) * t <= dist }
    fin - start
  end.inject(&:*)
end

def part_two(races)
  race = races.transpose.map {|r| r.inject {|a, b| "#{a}#{b}".to_i } }
  part_one [race]
end

races = parse_races STDIN.read

case ARGV[0]
when "one"
  p part_one(races)
when "two"
  p part_two(races)
end

