import org.scalatestplus.play.PlaySpec
import play.api.Logger

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.control.Breaks._

class OneHopfieldModelTest extends PlaySpec {

  def magnitude(vector: Vector[Double]): Double = vector.foldLeft(0.0)((res, e) => res + Math.pow(e, 2))

  def innerProduct(vector1: Vector[Double], vector2: Vector[Double]): Double = {
    require(vector1.length == vector2.length)

    vector1.indices.foldLeft(0.0)((res: Double, i: Int) => res + vector1(i) * vector2(i))
  }

  def outerProduct(vector1: Vector[Double], vector2: Vector[Double]): Array[Array[Double]] = {
    val vector1Length = vector1.length
    val vector2Length = vector2.length

    val matrix: Array[Array[Double]] = Array.ofDim[Double](vector1Length, vector2Length)

    for (i <- 0 until vector1Length)
      for (j <- 0 until vector2Length)
        matrix(i)(j) = vector1(i) * vector2(j)

    matrix
  }

  def matrixByScalarProduct(matrix: Array[Array[Double]], scalar: Double): Array[Array[Double]] = {
    for (i <- matrix.indices)
      for (j <- matrix(0).indices)
        matrix(i)(j) *= scalar

    matrix
  }

  def matrixByVectorInnerProduct(matrix: Array[Array[Double]], vector: Vector[Double]): Vector[Double] = {
    require(matrix(0).length == vector.length)

    val resultVector: ListBuffer[Double] = ListBuffer.fill(vector.length)(0)

    for (i <- matrix(0).indices)
      for (j <- matrix(0).indices)
        resultVector(i) += matrix(i)(j) * vector(j)

    resultVector.toVector
  }

  def buildMatrix(states: Array[Array[Double]]): Array[Array[Double]] = {
    val length = states(0).length
    val matrix: Array[Array[Double]] = Array.ofDim[Double](length, length)

    for (state <- states)
      for (i <- 0 until length)
        for (j <- 0 until length) {
          if (i != j) matrix(i)(j) += state(i) * state(j)
          if (matrix(i)(j) > 1) matrix(i)(j) = 1
        }

    matrix
  }

  def retrieve(matrix: Array[Array[Double]], signal: Vector[Double], sweep: Int): Vector[Double] = {
    val length = signal.length
    val resultState: ListBuffer[Double] = ListBuffer.fill(length)(0)

    breakable {
      for (s <- 0 until sweep) {
        for (i <- 0 until length) {
          val accumulator = (0 until length).foldLeft(0.0)((res, e) => res + matrix(i)(e)*signal(e))
          resultState(i) = if (accumulator == 0) 0 else 1

//          println(s)

          if (signal == resultState.toVector) {
            Logger.info(s"Retrieve was broken after $s rounds since the signal equals the resultState")
            break
          }
        }
      }
    }

    resultState.toVector
  }

  def flipBit(bit: Double): Double = bit match {
    case 0 => 1
    case 1 => 0
    case _ => throw new Exception("FlipBit found a bit that is not 0 or 1")
  }

  def flipVector(vector: Vector[Double], index: Int, numberOfBits: Int): Vector[Double] = {
    require(vector.length >= index + numberOfBits)

    val vectorArray = vector.toArray

    for (i <- 0 until numberOfBits) vectorArray(index + i) = flipBit(vectorArray(index + i))

    vectorArray.toVector
  }

  def giveAllFlippedCombinations(vector: Vector[Double], numberOfBits: Int): Seq[Vector[Double]] = {
    for (i <- 0 to vector.length-numberOfBits) yield flipVector(vector, i, numberOfBits)
  }

  def transferFunction(x: Double, lambda: Double) = {
    require(0 <= lambda && lambda <= 1)

    1/(1 + Math.exp(-lambda * x))
  }

  "A discrete Hopfield memory matrix" must {

    "be constructed" in {
      val states: Array[Array[Double]] = Array(
        Array(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
        Array(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0))

      val expectedMatrix = Array(
        Array(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
        Array(0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0),
        Array(0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0),
        Array(0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0),
        Array(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0),
        Array(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

      buildMatrix(states) mustBe expectedMatrix
    }
  }

  "A signal" must {

    "return its saved state" in {
      val matrix: Array[Array[Double]] = Array(
        Array(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
        Array(0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0),
        Array(0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0),
        Array(0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0),
        Array(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0),
        Array(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
      val savedState: Vector[Double] = Vector(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0)

      val signal: Vector[Double] = Vector(1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0)

      //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      //See why it is not broke and decrease sweep value does not change anything...
      //Sweep = 1 tjrs bon, est-ce lié ?
      //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      retrieve(matrix, signal, sweep = 8) mustBe savedState //sweep = 1 is enough here
    }

    "1.6.2: test" in {
      val states: Array[Array[Double]] = Array(
        Array(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0))

      val signal = states(0).clone()
      signal(5) = 1
      signal(8) = 1
      signal(60) = 1
      signal(90) = 1
      signal(120) = 1
      signal(150) = 1

      retrieve(matrix = buildMatrix(states), signal = states(0).toVector, sweep = 1) mustBe states(0).toVector
    }
  }

  "1.6.3 flipVector" must {

    "throw an exception when trying to flip bits out of index" in {
      val v: Vector[Double] = Vector(1, 0, 1)

      an [Exception] should be thrownBy flipVector(vector = v, index = 1, numberOfBits = 3)
    }

    "not throw an exception" in {
      val v: Vector[Double] = Vector(1, 0, 1)

      flipVector(vector = v, index = 1, numberOfBits = 2)
    }
  }

  "1.6.3 A vector" must {
    "have 1 bit flipped" in {
      val v: Vector[Double] = Vector(1, 0, 1, 0)

      val expectedVector: Vector[Double] = Vector(1, 0, 0, 0)

      flipVector(vector = v, index = 2, numberOfBits = 1) mustBe expectedVector
    }

    "have 3 bit flipped" in {
      val v: Vector[Double] = Vector(1, 0, 1, 0, 1, 0, 1, 0)

      val expectedVector: Vector[Double] = Vector(1, 1, 0, 1, 1, 0, 1, 0)

      flipVector(vector = v, index = 1, numberOfBits = 3) mustBe expectedVector
    }
  }

  "1.6.3 All combinations of flipped vectors" must {

    "be given" in {
      val v: Vector[Double] = Vector(1, 0, 1, 0, 1, 0)

      val expectedCombinations: Seq[Vector[Double]] = Seq(
        Vector(0, 1, 1, 0, 1, 0),
        Vector(1, 1, 0, 0, 1, 0),
        Vector(1, 0, 0, 1, 1, 0),
        Vector(1, 0, 1, 1, 0, 0),
        Vector(1, 0, 1, 0, 0, 1))

      giveAllFlippedCombinations(vector = v, numberOfBits = 2) mustBe expectedCombinations
    }
  }

  "1.6.3 retrieve" must {

    "retrieve signals with one flipped bit" in {
      val state: Vector[Double] = Vector(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0)
      val matrix = buildMatrix(Array(state.toArray))

      val oneBitFlippedVectors = giveAllFlippedCombinations(vector = state, numberOfBits = 1)

      for (oneBitFlippedVector <- oneBitFlippedVectors) {
        if (retrieve(matrix = matrix, signal = oneBitFlippedVector, sweep = 1) != state)
          Logger.info("Rappel de l'état :\n" + state + "\nVecteur incriminé :\n" + oneBitFlippedVector)
      }
    }

    "retrieve signals with two flipped bit" in {
      val state: Vector[Double] = Vector(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0)
      val matrix = buildMatrix(Array(state.toArray))

      val oneBitFlippedVectors = giveAllFlippedCombinations(vector = state, numberOfBits = 1)

      for (oneBitFlippedVector <- oneBitFlippedVectors) {
        if (retrieve(matrix = matrix, signal = oneBitFlippedVector, sweep = 1) != state)
          Logger.info("Rappel de l'état :\n" + state + "\nVecteur incriminé :\n" + oneBitFlippedVector)
      }
    }
  }

  "1.6.4 retrieve" must {

    //  "1.6.4.0 done before" in { }

    "1.6.4.1 transfer function (coder la fonction de transfert f(x) et l!expérimenter avec différentes valeurs de lambda)" in {

    }
  }
}
