#!/usr/bin/env ruby

class Map
  attr_accessor :name
  attr_accessor :maps

  def initialize(lines)
    @name = lines[0].split(' ')[0]
    @maps = lines[1..-1].map {|l| l.split(' ').map(&:to_i) }
  end

  def [](val)
    map = maps.find {|m| val >= m[1] && val <= (m[1] + m[2] - 1) }
    map ? map[0] + (val - map[1]) : val
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

def part_two(seeds, almanac)
  seeds = seeds.each_slice(2).map {|l, h| (l..l + h).to_a }.flatten

  p seeds.size
  part_one seeds, almanac
end

almanac = parse_almanac STDIN.read

case ARGV[0]
when "one"
  p part_one(*almanac)
when "two"
  p part_two(*almanac)
end

