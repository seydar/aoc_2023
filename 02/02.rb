#!/usr/bin/env ruby

class Game
  attr_accessor :id
  attr_accessor :revealed

  def initialize(id)
    @id       = id
    @revealed = {'red' => 0, 'green' => 0, 'blue' => 0}
  end
end

def parse_record(inp)
  inp.split("\n").map do |line|
    id_part, subset_part = line.split(": ")

    id   = id_part.split(' ')[-1].to_i
    game = Game.new id

    subset_part.split('; ').map do |subset|
      subset.split(', ').each do |dice|
        num, color = dice.split(' ')
        game.revealed[color] = [game.revealed[color], num.to_i].max
      end
    end

    game
  end
end

def part_one(record)
  limits = {'red' => 12, 'green' => 13, 'blue' => 14}
  possible = record.filter do |game|
    game.revealed.all? {|color, num| num <= limits[color] }
  end

  possible.sum {|g| g.id }
end

record = parse_record STDIN.read

case ARGV[0]
when "one"
  p part_one(record)
when "two"
  p part_two(record)
end

