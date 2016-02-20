import org.scalatestplus.play.PlaySpec

import scala.collection.mutable.ListBuffer

class OneVectorsAndMatricesOperationsTest extends PlaySpec {

  def magnitude(vector: Vector[Double]): Double = vector.foldLeft(0.0)((res, e) => res + Math.pow(e, 2))

  def innerProduct(vector1: Vector[Double], vector2: Vector[Double]): Double = {
    require(vector1.length == vector2.length)

    vector1.indices.foldLeft(0.0)((res: Double, i: Int) => res + vector1(i)*vector2(i))
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
        resultVector(i) += matrix(i)(j)*vector(j)

    resultVector.toVector
  }

  "The magnitude of a vector" must {

    "be calculated" in {
      val v: Vector[Double] = Vector(1, 7, 3, 2)

      val expectedMagnitude = Math.pow(1, 2) + Math.pow(7, 2) + Math.pow(3, 2) + Math.pow(2, 2)

      magnitude(v) mustBe expectedMagnitude
    }
  }

  "The inner product of two vectors" must {

    "throw an exception if there are not of the same size" in {
      val v1: Vector[Double] = Vector(7, 2)
      val v2: Vector[Double] = Vector(1, 7, 3)

      an[Exception] should be thrownBy innerProduct(v1, v2)
    }

    "be calculated" in {
      val v1: Vector[Double] = Vector(7, 2, 1)
      val v2: Vector[Double] = Vector(1, 7, 3)

      val expectedInnerProduct = 7*1 + 2*7 + 1*3

      innerProduct(v1, v2) mustBe expectedInnerProduct
    }
  }

  "The outer product of two vectors" must {

    "give the correct matrix" in {
      val v1: Vector[Double] = Vector(7, 2)
      val v2: Vector[Double] = Vector(1, 7, 3)

      /*
        |7*1=7 7*7=49 7*3=21|
        |2*1=2 2*7=14 2*3=6|
       */
      val expectedOuterProductMatrix: Array[Array[Double]] = Array(
        Array(7, 49, 21),
        Array(2, 14, 6))

      outerProduct(v1, v2).deep mustBe expectedOuterProductMatrix.deep
    }
  }

  "The product of a matrix by a scalar" must {

    "give the correct matrix" in {
      val scalar = 5
      val matrix: Array[Array[Double]] = Array(
        Array(7, 49, 21),
        Array(2, 14, 6))

      val expectedMatrix: Array[Array[Double]] = Array(
        Array(7 * scalar, 49 * scalar, 21 * scalar),
        Array(2 * scalar, 14 * scalar, 6 * scalar))

      matrixByScalarProduct(matrix, scalar).deep mustBe expectedMatrix.deep
    }
  }

  "The inner product of a matrix by a vector" must {

    "throw an exception if there are not of the same size" in {
      val v: Vector[Double] = Vector(1, 0)
      val matrix: Array[Array[Double]] = Array(
        Array(0, 0, 0, 0),
        Array(1, 0, 0, 0))

      an[Exception] should be thrownBy matrixByVectorInnerProduct(matrix, v)
    }

    "give the correct vector" in {
      val v: Vector[Double] = Vector(1, 0, 1, 0)
      val matrix: Array[Array[Double]] = Array(
        Array(0, 0, 0, 0),
        Array(1, 0, 0, 0),
        Array(0, 0, 1, 0),
        Array(0, 0, 0, 0))

      val expectedVector: Vector[Double] = Vector(0, 1, 1, 0)

      matrixByVectorInnerProduct(matrix, v) mustBe expectedVector
    }
  }
}