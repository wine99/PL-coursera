# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

require_relative './hw6provided'

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  # your enhancements here
  All_My_Pieces = All_Pieces +
              [
                rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2,1]]), # fst pattern
                [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]], # snd -||-
                [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]], # trd -||-
                rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2,1]])
              ]
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

end


class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheated = false
  end

  def subtract_score
    @score -= 100
  end

  def cheated
    @cheated
  end
  
  def cheated= x
    @cheated = x
  end

  def one_piece
    ran = @current_block.drop_by_one
    if !ran
      store_current
      if !game_over?
        @current_block = MyPiece.new([[0, 0]], self)
        @current_pos = nil
        @cheated = true
      end
    end
    @game.update_score
    draw
  end

  # # gets the next piece
  # def next_piece
  #   @current_block = MyPiece.next_piece(self)
  #   @current_pos = nil
  # end
end


class MyTetris < Tetris
  # your enhancements here

  def inject_cheat
    if @board.cheated
      @board.cheated = false
      run_game
    end
    if !@board.game_over? and @running
      @timer.stop
      @timer.start(@board.delay, (proc{@board.one_piece; inject_cheat}))
      # @timer.start(@board.delay, (proc{@board.run; inject_cheat}))
    end
  end

  def cheat
    #if @score >= 100
    @board.subtract_score
    update_score
    inject_cheat
    #end
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})
    @root.bind('c', proc {@board.delay-200; cheat})
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
    end
end