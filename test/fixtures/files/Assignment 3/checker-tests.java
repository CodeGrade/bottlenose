import java.awt.Color;
import javalib.worldimages.*;
import tester.*;

class ExamplesMobilesReference {
    Complex exampleComplex;
    Simple exampleSimple;

    Simple weight20 = new Simple(3, 20, Color.BLACK);
    Simple weight40 = new Simple(6, 40, Color.BLACK);
    Complex left = new Complex(4, 5, 50, this.weight20, this.weight40);
    Complex mobile = new Complex(4, 10, 5, left, this.weight20);

    ExamplesMobilesReference() {
        this.exampleComplex = 
                new Complex(1, 9, 3,
                        new Simple(1, 36, Color.BLUE),
                        new Complex(2, 8, 1,
                            new Simple(1, 12, Color.RED),
                            new Complex(2, 5, 3,
                                new Simple(2, 36, Color.RED),
                                new Simple(1, 60, Color.GREEN))));
        this.exampleSimple = new Simple(2, 20, Color.BLUE);
    }
    boolean testDrawMobile(Tester t) {
        // doesn't really do anything except check that I can call drawMobile
        // correctly
        WorldImage mobileImage = exampleComplex.drawMobile();
        boolean mobileImageIsImage = (mobileImage instanceof WorldImage);
        return t.checkExpect(mobileImageIsImage, true,
             "Does drawMobile return a WorldImage?");
    }
    void testStudentExamples(Tester t) {
        ExamplesMobiles studentExamples = new ExamplesMobiles();
        t.checkExpect(studentExamples.exampleSimple, this.exampleSimple,
             "Example simple mobile");
        t.checkExpect(studentExamples.exampleComplex, this.exampleComplex,
             "Example complex mobile");
    }
    void testCurWidth(Tester t) {
        t.checkExpect(this.exampleComplex.curWidth(), (36/20 + 1) + 9 + 3 + 1 + 3 + (60/20),
            "Width of the example complex mobile");
        t.checkExpect(this.exampleSimple.curWidth(), 20/10,
            "Width of the example simple mobile");
        t.checkExpect(this.mobile.curWidth(), 58, 
            "Width of very asymmetric mobile");
    }
    void testTotalWeight(Tester t) {
        t.checkExpect(this.weight20.totalWeight(), 20, 
            "Weight of a simple mobile");
        t.checkExpect(this.weight40.totalWeight(), 40, 
            "Weight of a simple mobile");
        t.checkExpect(this.left.totalWeight(), 60, 
            "Weight of a complex mobile");
        t.checkExpect(this.mobile.totalWeight(), 80,
            "Weight of a complex mobile");
        t.checkExpect(this.exampleComplex.totalWeight(), 36 + 12 + 36 + 60,
             "Weight of the example complex mobile");
        t.checkExpect(this.exampleSimple.totalWeight(), 20,
             "Weight of the example simple mobile");
    }
    void testTotalHeight(Tester t) {
        t.checkExpect(this.weight20.totalHeight(), 5,
            "Height of a simple mobile");
        t.checkExpect(this.weight40.totalHeight(), 10,
            "Height of a simple mobile");
        t.checkExpect(this.left.totalHeight(), 14,
            "Height of a complex mobile");
        t.checkExpect(this.mobile.totalHeight(), 18,
            "Height of a complex mobile");
        t.checkExpect(this.exampleSimple.totalHeight(), 4,
            "Height of the example simple mobile");
        t.checkExpect(this.exampleComplex.totalHeight(), 1 + 2 + 2 + 1 + (60/10),
            "Height of the example complex mobile");
    }
    void testIsBalanced(Tester t) {
        t.checkExpect(this.weight20.isBalanced(), true,
            "Is a simple mobile balanced?");
        t.checkExpect(this.weight40.isBalanced(), true,
            "Is a simple mobile balanced?");
        t.checkExpect(this.left.isBalanced(), false,
            "Is a complex mobile balanced?");
        t.checkExpect(this.mobile.isBalanced(), false,
            "Is a complex mobile balanced?");
        t.checkExpect(this.exampleSimple.isBalanced(), true,
            "Is the example simple mobile balanced?");
        t.checkExpect(this.exampleComplex.isBalanced(), true,
            "Is the example complex mobile balanced?");
    }
    void testBuildMobile(Tester t) {
        IMobile doubleComplex = 
            this.exampleComplex.buildMobile(this.exampleComplex, 4, 20);
        boolean doubleMobileIsComplex = (doubleComplex instanceof Complex);
        t.checkExpect(doubleMobileIsComplex, true, 
            "Result of buildMobile is a Complex mobile");
        if (doubleMobileIsComplex) {
            Complex doubleComplexMobile = (Complex)doubleComplex;
            t.checkExpect(
                doubleComplexMobile.leftside + doubleComplexMobile.rightside,
                20,
                "buildMobile should use the entire given strut width");
            t.checkExpect(
                doubleComplexMobile.leftside == doubleComplexMobile.rightside, 
                true,
                "Building a mobile with itself should be centered");
            t.checkExpect(doubleComplexMobile.isBalanced(), true,
                "Result of buildMobile should be balanced");
            t.checkExpect(doubleComplexMobile.totalHeight(),
                4 + this.exampleComplex.totalHeight(),
                "Result of buildMobile should use the height correctly");
        }
        IMobile tripleComplex =
            doubleComplex.buildMobile(this.exampleComplex, 10, 30);
        boolean tripleMobileIsComplex = (doubleComplex instanceof Complex);
        t.checkExpect(tripleMobileIsComplex, true, 
            "Result of buildMobile is a Complex mobile");
        if (tripleMobileIsComplex) {
            Complex tripleComplexMobile = (Complex)tripleComplex;
            t.checkExpect(
                tripleComplexMobile.leftside + tripleComplexMobile.rightside,
                30,
                "buildMobile should use the entire given strut width");
            t.checkExpect(
                2 * tripleComplexMobile.leftside == 
                    tripleComplexMobile.rightside,
                true,
                "If one side of a buildMobile result is twice as heavy as " + 
                "the other, the leftside should be half the rightside");
            t.checkExpect(tripleComplexMobile.isBalanced(), true,
                "Result of buildMobile should be balanced");
            t.checkExpect(tripleComplexMobile.totalHeight(),
                4 + 10 + this.exampleComplex.totalHeight(),
                "Result of buildMobile should use the height correctly");
        }
    }
}