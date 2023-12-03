#!/usr/bin/env ruby

class Map
  include Enumerable

  attr_accessor :grid

  def initialize(grid)
    @grid = parse_grid grid
  end

  def parse_grid(inp)
    grid = inp.split("\n").map {|l| l.split("") }

    @grid = grid.map do |row|
      current = nil # the number we're currently looking at

      row = row.map do |col|
        if col =~ /\d/
          current = Part.new unless current
          current.digits    << col
          current.locations << [row, col]
          current
        else
          current = nil
          col
        end
      end

      row
    end
  end

  def each(&b)
    grid.each_with_index do |row, y|
      row.each_with_index do |col, x|
        b.call col, x, y
      end
    end
  end

  def [](x, y)
    grid[y][x]
  end

  def touching(x, y)
    touching = (x - 1 .. x + 1).to_a.product (y - 1 .. y + 1).to_a
    touching.filter do |x, y|
      x >= 0 &&
        x < @grid[0].size &&
        y >= 0 &&
        y < @grid.size
    end.map {|x, y| self[x, y] }
  end
end

class Part
  attr_accessor :digits
  attr_accessor :locations

  def initialize
    @locations = []
    @digits    = []
  end

  def number
    digits.join("").to_i
  end

  def inspect
    "#<Part: #{number}>"
  end
end

SYMBOLS = ["+", "*", "%", "#", "/", "@", "$", "-", "&", "="]

def part_one(map)
  symbols = map.map {|c, x, y| SYMBOLS.include?(c) ? [x, y] : nil }.compact

  parts   = symbols.map do |x, y|
    map.touching(x, y).filter {|o| o.is_a? Part }.uniq
  end

  parts.flatten.sum {|p| p.number }
end

def part_two(map)
  gears = map.map {|c, x, y| c == "*" ? [x, y] : nil }.compact

  potential = gears.map do |x, y|
    map.touching(x, y).filter {|o| o.is_a? Part }.uniq
  end

  gears = potential.filter {|ps| ps.size == 2}
  gears.map {|ps| ps.map(&:number).inject(&:*) }.sum
end

def parse_map(inp)
  Map.new inp
end

map = parse_map STDIN.read

case ARGV[0]
when "one"
  p part_one(map)
when "two"
  p part_two(map)
end

