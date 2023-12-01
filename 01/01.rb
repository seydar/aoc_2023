#!/usr/bin/env ruby

def part_one(map)
  digits = map.map do |chars|
    chars.split("")
         .filter {|c| c =~ /\d/ }
  end

  cals = digits.map {|ds| (ds[0] + ds[-1]).to_i }
  cals.sum
end

WORDS = {"one"   => "1",
         "two"   => "2",
         "three" => "3",
         "four"  => "4",
         "five"  => "5",
         "six"   => "6",
         "seven" => "7",
         "eight" => "8",
         "nine"  => "9"}
('0'..'9').each {|d| WORDS[d] = d }

def part_two(map)
  pairs = map.map do |str|
    # Find the first
    positions  = WORDS.map {|word, _| [word, str.index(word)] }.filter {|w, p| p }
    positions += ('0'..'9').map {|i| [i, str.index(i)] }.filter {|d, p| p }
    first = positions.min_by {|w, p| p }

    # using rindex to search from the back first
    positions  = WORDS.map {|word, _| [word, str.rindex(word)] }.filter {|w, p| p }
    positions += ('0'..'9').map {|i| [i, str.rindex(i)] }.filter {|d, p| p }
    last  = positions.max_by {|w, p| p }

    (WORDS[first[0]] + WORDS[last[0]]).to_i
  end

  pairs.sum
end

def parse_map(inp)
  inp.split("\n")
end

map = parse_map STDIN.read

case ARGV[0]
when "one"
  p part_one(map)
when "two"
  p part_two(map)
end

