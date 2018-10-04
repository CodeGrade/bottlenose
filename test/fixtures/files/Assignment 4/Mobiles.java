
// Assignment 3 - problem 2
// hunter mark, henry jones
// mark2020, joneshe

import java.awt.Color;

import javalib.funworld.WorldScene;
import javalib.worldcanvas.WorldCanvas;
import javalib.worldimages.AboveAlignImage;
import javalib.worldimages.AboveImage;
import javalib.worldimages.AlignModeX;
import javalib.worldimages.AlignModeY;
import javalib.worldimages.BesideAlignImage;
import javalib.worldimages.LineImage;
import javalib.worldimages.OutlineMode;
import javalib.worldimages.OverlayOffsetAlign;
import javalib.worldimages.Posn;
import javalib.worldimages.RectangleImage;
import javalib.worldimages.TextImage;
import javalib.worldimages.WorldImage;
import tester.Tester;

//represent a mobile piece of art  
interface IMobile {

  //computes the total weight of the mobile
  int totalWeight();

  //computes the height of the mobile
  //Simple mobiles have a height of their weight divided by 10 (rounded down)
  int totalHeight();

  //computes whether a mobile is balanced i.e. no net torque 
  boolean isBalanced();

  //combines this balanced mobile with the given balanced mobile and produces a new mobile
  IMobile buildMobile(IMobile that, int length, int strutLength);

  //computes the current width of a mobile
  int curWidth();

  //computes the current width of a mobile left side 
  int curWidthHelpLeft();

  //computes the current width of a mobile right side 
  int curWidthHelpRight();

  //produces the image of this mobile
  WorldImage drawMobile();

}

//represents a struct with a single mobile i.e. a simple mobile 
class Simple implements IMobile {
  int length; //length refers to the length of the vertical string
  int weight;
  Color color;

  Simple(int length, int weight, Color color) {
    this.length = length;
    this.weight = weight;
    this.color = color;
  }

  /*
   * TEMPLATE FIELDS: 
   * ... this.length ... -- int
   * ... this.weight ... -- int
   * ... this.color ... -- color 
   * METHODS 
   * ... this.totalWieght() ... -- int
   * ... this.totalHeight() ... --int
   * ... this.isBalanced() ... -- bool
   * ... this.buildMobile(IMobile, int, int) ... -- IMobile
   * ... this.curWidth() ... --int
   * ... this.curWidthHelpLeft() ... --- int
   *  ... this.curWidthHelpRight() ... --- int
   * 
   * METHODS FOR FIELDS 
   * NONE 
   */

  //computes the total weight of the mobile
  public int totalWeight() {
    return this.weight;
  }

  //computes the height of the mobile
  //Simple mobiles have a height of their weight divided by 10 (rounded down)
  public int totalHeight() {
    return this.length + this.weight / 10;
  }

  //computes whether a mobile is balanced i.e. no net torque, a simple is always blanced 
  public boolean isBalanced() {
    return true;
  }

  //combines this balanced mobile with the given balanced mobile and produces a new mobile
  public IMobile buildMobile(IMobile that, int length, int strutLength) {
    int rightside = (this.totalWeight() * strutLength) / (that.totalWeight() + this.totalWeight());
    int leftside = strutLength - rightside;
    return new Complex(length, leftside, rightside, this, that);
  }

  //computes the current width of a simple mobile
  public int curWidth() {
    if (this.weight % 10 > 0) {
      return this.weight / 10 + 1;
    }
    else {
      return this.weight / 10;
    }
  }

  //computes current width of the left side of simple mobile in a complex piece of art 
  public int curWidthHelpLeft() {
    if (this.weight % 20 > 0) {
      return this.weight / 20 + 1;
    }
    else {
      return this.weight / 20;
    }

  }

  //computes current width of the right side of simple mobile in a complex piece of art 
  public int curWidthHelpRight() {
    return this.curWidthHelpLeft();
  }

  //produces the image of this mobile
  public WorldImage drawMobile() {
    TextImage txt = new TextImage(Integer.toString(weight), 3, Color.BLACK);
    return (new AboveImage(new LineImage(new Posn(0, length), Color.BLACK),
        new RectangleImage(curWidth(), weight / 10, OutlineMode.SOLID, color),
        txt).movePinhole(0,
            this.totalHeight() / 2));
  }

}

//represents a struct with 2 mobile i.e. a complex mobile 
class Complex implements IMobile {
  int length; //length refers to the length of the vertical string
  int leftside; // Horizontal 
  int rightside; // Horizontal 
  IMobile left;
  IMobile right;

  Complex(int length, int leftside, int rightside, IMobile left, IMobile right) {
    this.length = length;
    this.leftside = leftside;
    this.rightside = rightside;
    this.left = left;
    this.right = right;
  }

  /*
   * TEMPLATE FIELDS: 
   * ... this.length ... -- int
   * ... this.leftside ... -- int
   * ... this.rightside ... -- int
   * ... this.left ... -- IMobile 
   * ... this.right ... -- IMobile 
   * 
   * METHODS 
   * ... this.totalWieght() ... -- int
   * ... this.totalHeight() ... --int
   * ... this.isBalanced() ... -- bool
   * ... this.buildMobile(IMobile, int, int) ... -- IMobile
   * ... this.curWidth() ... --int
   * ... this.curWidthHelpLeft() ... --- int
   *  ... this.curWidthHelpRight() ... --- int
   * 
   * METHODS FOR FIELDS 
   * ... this.right/left.totalWieght() ... -- int
   * ... this.right/left.totalHeight() ... --int
   * ... this.right/left.isBalanced() ... -- bool
   * ... this.right/left.buildMobile(IMobile, int, int) ... -- IMobile
   * ... this.right/left.curWidth() ... --int
   * ... this.right/left.curWidthHelpLeft() ... --- int
   *  ... this.right/left.curWidthHelpRight() ... --- int
   */

  //computes the total weight of the mobile
  public int totalWeight() {
    return this.left.totalWeight() + this.right.totalWeight();
  }

  //computes the height of the mobile
  //Simple mobiles have a height of their weight divided by 10 (rounded down)
  public int totalHeight() {
    return this.length + Math.max(this.left.totalHeight(), this.right.totalHeight());
  }

  //computes whether a mobile is balanced i.e. no net torque 
  public boolean isBalanced() {
    return this.left.totalWeight() * this.leftside == this.right.totalWeight() * this.rightside;
  }

  //combines this balanced mobile with the given balanced mobile and produces a new mobile
  public IMobile buildMobile(IMobile that, int length, int strutLength) {
    int rightside = ((this.totalWeight() * strutLength)
        / (that.totalWeight() + this.totalWeight()));
    int leftside = strutLength - rightside;
    return new Complex(length, leftside, rightside, this, that);
  }

  //Computes the curWidth by travising down the mobile to measure the left and right side length 
  public int curWidth() {
    return this.curWidthHelpLeft() + this.curWidthHelpRight();
  }

  //computes the max width of the left side of a mobile
  public int curWidthHelpLeft() {
    return Math.max(this.leftside + this.left.curWidthHelpLeft(),
        -this.rightside + this.right.curWidthHelpLeft());
  }

  // computes the max width of the right side of a mobile
  public int curWidthHelpRight() {
    return Math.max(this.rightside + this.right.curWidthHelpRight(),
        -this.leftside + this.left.curWidthHelpRight());
  }

  //produces the image of this complex mobile
  public WorldImage drawMobile() {
    return new AboveAlignImage(AlignModeX.PINHOLE,
        (new BesideAlignImage(AlignModeY.BOTTOM,
            new LineImage(new Posn(-this.leftside, 0), Color.BLACK),
            new LineImage(new Posn(0, this.length), Color.BLACK),
            new LineImage(new Posn(this.rightside, 0), Color.BLACK))
                .movePinholeTo(new Posn(((this.leftside - this.rightside)), 0))),
        new OverlayOffsetAlign(AlignModeX.PINHOLE, AlignModeY.TOP,
            this.left.drawMobile().movePinhole(0, 0), this.leftside + this.rightside, 0,
            this.right.drawMobile().movePinhole(0, 0)).movePinhole(0, 0));
  }
}

//
//Examples of Mobiles
class ExamplesMobiles {
  IMobile exampleSimple = new Simple(2, 20, Color.BLUE);
  IMobile exampleComplex = new Complex(1, 9, 3, new Simple(1, 36, Color.BLUE),
      new Complex(2, 8, 1, new Simple(1, 12, Color.RED),
          new Complex(2, 5, 3, new Simple(2, 36, Color.RED), new Simple(1, 60, Color.GREEN))));
  IMobile example3 = new Complex(1, 3, 10, this.exampleComplex, new Simple(1, 55, Color.BLUE));

  IMobile exampleComplex2 = new Complex(1, 9, 3, new Simple(1, 36, Color.BLUE),
      new Simple(1, 12, Color.RED));

  IMobile exampleFarLeft = new Complex(1, 9, 3, new Simple(1, 36, Color.BLUE),
      new Complex(2, 8, 1, new Simple(1, 12, Color.RED),
          new Complex(2, 1000, 3, new Simple(2, 36, Color.RED), new Simple(1, 60, Color.GREEN))));

  IMobile exampleSimpleAndExampleComplex = new Complex(3, 144, 20, exampleSimple, exampleComplex);
  IMobile exampleComplexAndExampleSimple = new Complex(3, 20, 144, exampleComplex, exampleSimple);
  IMobile exampleComplexAndExampleComplex = new Complex(4, 10, 10, exampleComplex, exampleComplex);

  boolean testTotalWeight(Tester t) {
    return t.checkExpect(this.exampleSimple.totalWeight(), 20)
        && t.checkExpect(this.exampleComplex.totalWeight(), 144)
        && t.checkExpect(this.example3.totalWeight(), 199);
  }

  boolean testTotalHeight(Tester t) {
    return t.checkExpect(this.exampleSimple.totalHeight(), 4)
        && t.checkExpect(this.exampleComplex.totalHeight(), 12)
        && t.checkExpect(this.example3.totalHeight(), 13);
  }

  boolean testIsBalanced(Tester t) {
    return t.checkExpect(this.exampleSimple.isBalanced(), true)
        && t.checkExpect(this.exampleComplex.isBalanced(), true)
        && t.checkExpect(this.example3.isBalanced(), false)
        && t.checkExpect(this.exampleSimple.buildMobile(exampleComplex, 3, 328).isBalanced(), true)
        && t.checkExpect(this.exampleSimple.buildMobile(exampleComplex, 3, 164).isBalanced(), true);
  }

  boolean testBuildMobile(Tester t) {
    return t.checkExpect(this.exampleSimple.buildMobile(this.exampleComplex, 3, 164),
        this.exampleSimpleAndExampleComplex)
        && t.checkExpect(this.exampleComplex.buildMobile(this.exampleSimple, 3, 164),
            this.exampleComplexAndExampleSimple)
        && t.checkExpect(exampleComplex.buildMobile(this.exampleComplex, 4, 20),
            this.exampleComplexAndExampleComplex);
  }

  boolean testCurWidth(Tester t) {
    return t.checkExpect(this.exampleSimple.curWidth(), 2)
        && t.checkExpect(this.exampleComplex.curWidth(), 21)
        && t.checkExpect(this.exampleFarLeft.curWidth(), 1008);
  }

  boolean testCurWidthRight(Tester t) {
    return t.checkExpect(this.exampleComplex.curWidthHelpRight(), 10)
        && t.checkExpect(this.exampleFarLeft.curWidthHelpRight(), 10)
        && t.checkExpect(this.exampleSimple.curWidthHelpRight(), 1);
  }

  boolean testCurWidthLeft(Tester t) {
    return t.checkExpect(this.exampleComplex.curWidthHelpLeft(), 11)
        && t.checkExpect(this.exampleFarLeft.curWidthHelpLeft(), 998)
        && t.checkExpect(this.exampleSimple.curWidthHelpLeft(), 1);
  }

  boolean testDrawMobile(Tester t) {
    WorldCanvas c = new WorldCanvas(500, 500);
    WorldScene s = new WorldScene(500, 500);
    return c.drawScene(s.placeImageXY(this.exampleComplex.drawMobile(), 15, 15)) && c.show();
  }

}
