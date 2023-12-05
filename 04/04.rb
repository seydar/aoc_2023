#!/usr/bin/env ruby

class Card
  attr_accessor :id
  attr_accessor :winning
  attr_accessor :have

  def initialize(line)
    parse_input line
  end

  def parse_input(line)
    id, nums = line.split ': '

    @id = id.split(' ')[1].to_i

    @winning, @have = nums.split(' | ').map {|r| r.split(' ').map(&:to_i) }
  end

  def matching
    winning & have
  end

  def score
    matching.empty? ? 0 : 2 ** (matching.size - 1)
  end

  def inspect
    "#<Card: #{id}>"
  end
end

def parse_cards(inp)
  inp.split("\n").map {|l| Card.new l }
end

def part_one(cards)
  cards.sum {|c| c.score }
end

def part_two(cards)
  tally = cards.map {|c| [c, 1] }.to_h

  cards.each do |cur|
    copies = cards[cur.id, cur.matching.size]
    copies.each {|c| tally[c] += tally[cur] }
  end

  tally.values.sum
end

cards = parse_cards STDIN.read

case ARGV[0]
when "one"
  p part_one(cards)
when "two"
  p part_two(cards)
end

