package game_theory.utils

import org.ejml.simple.SimpleMatrix

import scala.annotation.tailrec
import scala.util.{Success, Try}

case class Matrix(matrix: Array[Array[Double]]) {

    @tailrec
    private def determinantHandler(coefficientsAndMatricesMinors: Array[(Double, Matrix)]): Double = {
        coefficientsAndMatricesMinors match {
            case Array((_, Matrix(Array())), _*) =>
                coefficientsAndMatricesMinors.map(_._1).sum
            case _ =>
                val newCoefficientsAndMatricesMinors = coefficientsAndMatricesMinors.par.flatMap { case (coefficient, matrixMinor) =>
                    if (matrixMinor.getMatrixRowsCount == 1 && matrixMinor.getMatrixColumnsCount == 1) {
                        Array((coefficient * matrixMinor.matrix.head.head, Matrix(Array())))
                    } else {
                        matrixMinor.matrix.head.zipWithIndex.par.map { case (matrixElement, columnIndex) =>
                            val coefficientMatrixMinor = if (columnIndex % 2 == 0) matrixElement else -matrixElement
                            val matrixMinorOfMatrixMinor = matrixMinor.copy(
                                matrixMinor.matrix.tail
                                    .map(matrixMinorRow => matrixMinorRow.patch(columnIndex, Nil, 1))
                            )
                            (coefficient * coefficientMatrixMinor, matrixMinorOfMatrixMinor)
                        }
                    }
                }.toArray
                determinantHandler(newCoefficientsAndMatricesMinors)
        }
    }

    private def determinantHandlerBig(matrix: Matrix): Double = {
        new SimpleMatrix(matrix.matrix).determinant()
    }

    // Laplace expansion
    private def determinant: Double = {
        val matrixColumnsCount = this.getMatrixColumnsCount
        val matrixRowsCount = this.getMatrixRowsCount
        if (matrixColumnsCount > 0 && matrixColumnsCount == matrixRowsCount) {
            val det =
                if (matrixColumnsCount > 5) determinantHandlerBig(this)
                else determinantHandler(Array((1.0, this)))
            det
        } else {
            throw new Exception("Матрица не является квадратной")
        }
    }

    def IsNonDegenerateMatrix: Boolean = Try(determinant) match {
        case Success(det) if det != 0.0 => true
        case _ => false
    }

    def getMatrixRowsCount: Int = matrix.length

    def getMatrixColumnsCount: Int = {
        val matrixColumnsCounts = matrix.map(matrixRow => matrixRow.length)
        matrixColumnsCounts.distinct match {
            case Array(matrixColumnsCount) => matrixColumnsCount
            case Array() => 0
            case Array(_*) => throw new Exception("Матрица задана неверно. Количество столбцов в разных строках отличается")
        }
    }

    def *(otherMatrix: Matrix): Matrix = {
        val matrixColumnsCount = this.getMatrixColumnsCount
        val otherMatrixRowsCount = otherMatrix.getMatrixRowsCount
        if (matrixColumnsCount > 0 && matrixColumnsCount == otherMatrixRowsCount) {
            val otherMatrixTransposed = otherMatrix.transpose
            val matricesMultiplicationResult = matrix.map { matrixRow =>
                otherMatrixTransposed.matrix.map { otherMatrixRow =>
                    matrixRow.zip(otherMatrixRow).map { case (matrixElement, otherMatrixElement) =>
                        matrixElement * otherMatrixElement
                    }.sum
                }
            }
            this.copy(matricesMultiplicationResult)
        } else {
            throw new Exception("Свойство согласованности матриц не выполняется")
        }
    }

    def +(otherMatrix: Matrix): Matrix = {
        val (matrixRowsCount, matrixColumnsCount) = (this.getMatrixRowsCount, this.getMatrixColumnsCount)
        val (otherMatrixRowsCount, otherMatrixColumnsCount)  = (otherMatrix.getMatrixRowsCount, otherMatrix.getMatrixColumnsCount)
        if (matrixRowsCount > 0 && matrixColumnsCount > 0 &&
            matrixRowsCount == otherMatrixRowsCount && matrixColumnsCount == otherMatrixColumnsCount) {
            val matricesAdditionResult = matrix.zip(otherMatrix.matrix).map { case (matrixRow, otherMatrixRow) =>
                matrixRow.zip(otherMatrixRow).map { case (matrixElement, otherMatrixElement) =>
                    matrixElement + otherMatrixElement
                }
            }
            this.copy(matricesAdditionResult)
        } else {
            throw new Exception("Матрицы разных размеров нельзя сложить")
        }
    }

    def -(otherMatrix: Matrix): Matrix = {
        this + otherMatrix.copy(otherMatrix.matrix.map(_.map(elem => -1.0 * elem)))
    }

    def transpose: Matrix = this.copy(matrix.transpose)

    def inverse: Matrix = {
        val matrixDeterminant = this.determinant
        if (matrixDeterminant == 0) throw new Exception("Определитель матрицы равен 0")
        val matrixAdjugate = matrix.zipWithIndex.map { case (matrixRow, rowIndex) =>
            matrixRow.zipWithIndex.map { case (_, columnIndex) =>
                val coefficient = math.pow(-1, rowIndex + columnIndex)
                val matrixMinor = matrix.patch(rowIndex, Nil, 1).map(matrixMinorRow => matrixMinorRow.patch(columnIndex, Nil, 1))
                if (matrixMinor.isEmpty) coefficient
                else coefficient * this.copy(matrixMinor).determinant
            }
        }.transpose
        this.copy(matrixAdjugate.map(_.map(_ * (1 / matrixDeterminant))))
    }

}

object Matrix {
    def fromArray(matrixStrategy: Array[Array[String]]): Either[String, Matrix] = {
        Try(
            Right(Matrix(matrixStrategy.map(_.map(Utils.convertExpressionToNumber))))
        ).recover {
            case exception: Exception =>
                val errorMessage = exception.getMessage
                game_theory.logger.error("Matrix elements convert error: " + errorMessage)
                Left(errorMessage)
        }.get
    }

    def fill(rowsCount: Int, columnsCount: Int, element: Double): Matrix =
        Matrix(Array.fill[Array[Double]](rowsCount)(Array.fill[Double](columnsCount)(element)))
}
