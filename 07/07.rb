#!/usr/bin/env ruby

class Hand
  KINDS = [:high_card,
           :one_pair,
           :two_pair,
           :three_of_kind,
           :full_house,
           :four_of_kind,
           :five_of_kind]

  RANKS = "J23456789TJQKA".split("")

  attr_accessor :cards
  attr_accessor :bet

  def initialize(cards, bet)
    @bet   = bet.to_i
    @cards = cards.split("")
  end

  def kind
    return @kind if @kind

    freqs = cards.inject({}) {|h, k| h[k] ||= 0;  h[k] += 1; h }
    freqs.delete "J"

    new_cards = cards.dup
    while new_cards.include? "J"
      should_be = freqs.max_by {|k, v| v } || ["A"]
      should_be = should_be[0]
      new_cards[new_cards.index("J")] = should_be

      freqs = new_cards.inject({}) {|h, k| h[k] ||= 0;  h[k] += 1; h }
      freqs.delete "J"
    end

    @kind = if freqs.values.max == 5
              :five_of_kind
            elsif freqs.values.max == 4
              :four_of_kind
            elsif freqs.values.max == 3 && freqs.values.min == 2
              :full_house
            elsif freqs.values.max == 3
              :three_of_kind
            elsif freqs.values.count {|f| f == 2 } == 2
              :two_pair
            elsif freqs.values.max == 2
              :one_pair
            else
              :high_card
            end
  end

  def <=>(other)
    rank = KINDS.index(kind) <=> KINDS.index(other.kind)

    if rank == 0
      cards.map {|c| RANKS.index c } <=> other.cards.map {|c| RANKS.index c }
    else
      rank
    end
  end
end

def part_one(hands)
  hands[0].kind
  gets
  hands.sort.map.with_index {|h, i| h.bet * (i + 1) }.sum
end

def part_two(hands)
  hands.sort.map.with_index {|h, i| h.bet * (i + 1) }.sum
end

def parse_hands(inp)
  inp.split("\n").map {|s| Hand.new(*s.split(" ")) }
end

hands = parse_hands STDIN.read

case ARGV[0]
when "one"
  p part_one(hands)
when "two"
  p part_two(hands)
end

