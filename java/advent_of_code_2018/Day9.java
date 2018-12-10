package advent_of_code_2018;

import java.util.List;
import java.util.ArrayList;

public class Day9 {

  class Game {
    private Node current;
    private int size;
    private boolean done;
    private int maxMarble;
    private int player;
    private long[] scores;
    private int lastMarble;
    

    Game(int playerCount, int maxMarble) {
      current = new Node(0);
      size = 1;
      this.done = false;
      this.maxMarble = maxMarble;
      this.player = 0;
      this.scores = new long[playerCount];
      this.lastMarble = 0;
    }

    long maxScore() {
      long max = 0;
      for(long s : scores) {
        if (s > max) {
          max = s;
        }
      }

      return max;
    }

    void play() {
      while(!done) {
        addNext();
      }
    }

    void addNext() {
      int m = lastMarble + 1;
      if (m > maxMarble) {
        done = true;
        return;
      }

      if (m % 23 == 0) {
        moveCCW();
        moveCCW();
        moveCCW();
        moveCCW();
        moveCCW();
        moveCCW();
        moveCCW();
        scores[player] += (current.marble + m);
        remove();
      } else {
        add(m);
      }

      lastMarble = m;
      player = (player + 1) % scores.length;
    }

    void add(long marble) {
      moveCW();
      Node newNode = new Node(marble);
      newNode.next = current.next;
      newNode.prev = current;

      current.next = newNode;

      newNode.next.prev = newNode;

      current = newNode;
      size++;
    }

    void remove() {
      Node p = current.prev;
      Node n = current.next;
      p.next = n;
      n.prev = p;
      current = n;
      size--;
    }

    void CCWSeven() {
      moveCCW();
      moveCCW();
      moveCCW();
      moveCCW();
      moveCCW();
      moveCCW();
      moveCCW();
    }

    void moveCCW() {
      current = current.prev;
    }

    void moveCW() {
      current = current.next;
    }

    int player() {
      return player;
    }

    class Node {
      public Node prev;
      public Node next;
      public final long marble;

      Node(long marble) {
        this.prev = this;
        this.next = this;
        this.marble = marble;
      }
    } 

    Node findZero() {
      Node n = current;
      while(n.marble != 0) {
        n = n.next;
      }

      return n;
    }

    @Override
    public String toString() {
      StringBuffer sb = new StringBuffer();
      Node n = findZero(); 

      for(int i = 0; i < size; i++) {
        if (n == current) {
          sb.append("(" + n.marble + ")");

        } else {
          sb.append(n.marble);
        }

        sb.append(" ");
        n = n.next;
      }

      return sb.toString();
    }
  }

  public long play(int playerCount, int maxMarble) {
    Game game = new Game(playerCount, maxMarble);
    game.play();
    return game.maxScore();
  }

  public void printTransitions() {
    Game game = new Game(9, 25);
    for(int i = 0; i < 25; i++) {
      System.out.print("[" + (game.player() + 1) + "]  ");
      game.addNext();
      System.out.println(game);
    }
  }

  public void test() throws Exception {
    assertScore(play(9, 25), 32);
    assertScore(play(10, 1618), 8317);
    assertScore(play(13, 7999), 146373);
    assertScore(play(17, 1104), 2764);
    assertScore(play(21, 6111), 54718);
    assertScore(play(30, 5807), 37305);
  }

  public void part1() {
    long s = System.currentTimeMillis();
    System.out.println("Part 1: " +  play(459, 71790) + " in " + (System.currentTimeMillis() - s) + "ms");
  }

  public void part2() {
    long s = System.currentTimeMillis();
    System.out.println("Part 2: " +  play(459, 7179000) + " in " + (System.currentTimeMillis() - s) + "ms");
  }

  public void assertScore(long actualScore, long expectedScore) throws Exception {
    if (actualScore != expectedScore) {
      throw new Exception("Expected: " + expectedScore + ", got " + actualScore);
    }
  }
}
