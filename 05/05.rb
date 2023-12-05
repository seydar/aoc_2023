#!/usr/bin/env ruby

class Range

  def overlap?(other)
    include?(other.begin) || other.include?(self.begin)
  end

  def combine(other)
    # if overlapping
    if overlap?(other)
      [self.begin, other.begin].min .. [self.end, other.end].max
    end
  end
end

class Map
  attr_accessor :name
  attr_accessor :maps

  def initialize(lines)
    @name = lines[0].split(' ')[0]
    @maps = lines[1..-1].map {|l| l.split(' ').map(&:to_i) }
  end

  def [](val)
    map = maps.find {|m| val >= m[1] && val < (m[1] + m[2]) }
    map ? map[0] + (val - map[1]) : val
  end

  def rev(val)
    map = maps.find {|m| val >= m[0] && val < (m[0] + m[2]) }
    map ? map[1] + (val - map[0]) : val
  end
end

def parse_almanac(inp)
  seeds, *maps = inp.split("\n\n")

  seeds = seeds.split(" ")[1..-1].map(&:to_i)

  [seeds, maps.map {|m| Map.new m.split("\n") }]
end

def part_one(seeds, almanac)
  locations = seeds.map do |seed|
    almanac.inject(seed) {|val, map| map[val] }
  end
  locations.min
end

def collapse_ranges(ranges)
  combined = []
  (0..ranges.size - 1).each do |i|
    added = false

    (i + 1..ranges.size - 1).each do |j|
      if ranges[i].overlap? ranges[j]
        combined << ranges[i].combine(ranges[j])
        added = true
      end
    end

    combined << ranges[i] unless added
  end

  combined
end

# gotta work backwards
def part_two(seeds, almanac)
  seeds = seeds.each_slice(2).map {|l, h| (l .. l + h) }
  
  #seeds = collapse_ranges seeds

  0.upto(seeds.map(&:end).max).map do |i|
    p i if i % 1_000_000 == 0
    origin = almanac.reverse.inject(i) {|val, map| map.rev val }
    return [origin, i] if seeds.find {|r| r.include? origin }
  end
end

almanac = parse_almanac STDIN.read

case ARGV[0]
when "one"
  p part_one(*almanac)
when "two"
  p part_two(*almanac)
end

