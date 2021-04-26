package game_theory.utils

import game_theory.{MSA, logger}

import scala.util.Try

// polynomial = ax^2 + by^2 + cxy + dx + ey
case class GameCorePolynomial(polynomial: Array[(Double, Array[Int])]) {

    // Метод вычисления производной
    def derivative(variableIndex: Int): GameCorePolynomial = this.copy(
        polynomial.flatMap { case (coefficient, variablesDegrees) =>
            val variableDegree = variablesDegrees(variableIndex)
            if (variableDegree == 0) None
            else Some((coefficient * variableDegree, variablesDegrees.updated(variableIndex, variableDegree - 1)))
        }
    )

    // Значение функции от аргумента
    def compute(variablesValues: Array[Double]): Double = {
        Utils.number2number(polynomial.foldLeft(0.0) { case (value, (coefficient, variablesDegrees)) =>
            value +
            (coefficient *
            variablesDegrees.zip(variablesValues).map { case (degree, value) => math.pow(value, degree) }.product)
        })
    }

    def +(otherGameCorePolynomial: GameCorePolynomial): GameCorePolynomial = {
        val (polynomialsSum, otherGameCorePolynomialUpdated) =
            polynomial.foldLeft((Array.empty[(Double, Array[Int])], otherGameCorePolynomial.polynomial)) {
                case ((polynomialsSum, otherPolynomial), polynomialElement) =>
                    val (polynomialsSumElement, otherPolynomialUpdated) =
                        otherPolynomial.zipWithIndex.find(_._1._2 sameElements polynomialElement._2) match {
                            case Some(((otherCoefficient, variablesDegrees), index)) =>
                                ((polynomialElement._1 + otherCoefficient, variablesDegrees),
                                    otherPolynomial.patch(index, Nil, 1))
                            case None =>
                                (polynomialElement, otherPolynomial)
                        }
                    (polynomialsSum :+ polynomialsSumElement, otherPolynomialUpdated)
            }
        this.copy(polynomialsSum ++ otherGameCorePolynomialUpdated)
    }

}

object GameCorePolynomial {

    def fromArray(polynomialAsArray: Array[MSA]): Either[String, GameCorePolynomial] = Try {
        val polynomial = polynomialAsArray.map { msa =>
            val coefficient = msa.getOrElse("coefficient", throw new Exception("Не задан коэффициент"))
            val variablesDegrees = msa.getOrElse("variablesDegrees", throw new Exception("Не заданы степени переменных"))
            (
                Utils.convertExpressionToNumber(coefficient.toString),
                variablesDegrees.asInstanceOf[List[String]].map(_.toInt).toArray
            )
        }
        Right(GameCorePolynomial(polynomial))
    }.recover {
        case exception: Exception =>
            val errorMsg = exception.getMessage
            game_theory.logger.error(errorMsg)
            Left(errorMsg)
    }.get

    // Решение системы методом Гаусса
    def solve(gameCorePolynomials: Array[GameCorePolynomial]): Array[Double] = {
        val matrix = gameCorePolynomials.map { gameCorePolynomial =>
            gameCorePolynomial.copy(
                gameCorePolynomial.polynomial.init :+
                (0.0 - gameCorePolynomial.polynomial.last._1, gameCorePolynomial.polynomial.last._2)
            )
        }
        val polynomialsDegrees = matrix.head.polynomial.map(_._2)
        val matrixSorted = matrix.map(gameCorePolynomial => gameCorePolynomial.copy(
            polynomialsDegrees.flatMap { polynomialDegrees =>
                gameCorePolynomial.polynomial.find(_._2 sameElements polynomialDegrees)
            }
        ))
        game_theory.logger.info("\nХод решения системы уравнений методом Гаусса:\n" + Utils.formatTable(matrixSorted.map(_.polynomial.map(_._1).toSeq).toSeq))
        val rowsIndices = matrixSorted.zipWithIndex.map(_._2)
        val updatedMatrix = rowsIndices.foldLeft(matrixSorted) { case (updatedMatrix, rowIndex) =>
            val polynomialRow = updatedMatrix(rowIndex)
            val coefficient = polynomialRow.polynomial(rowIndex)._1
            val newMatrix = updatedMatrix.zipWithIndex.map { case (subPolynomialRow, subRowIndex) =>
                if (rowIndex == subRowIndex) {
                    subPolynomialRow
                } else {
                    val correspondCoefficient = subPolynomialRow.polynomial(rowIndex)._1
                    val factor = correspondCoefficient / coefficient
                    subPolynomialRow.copy(
                        subPolynomialRow.polynomial zip polynomialRow.polynomial map {
                            case ((subCoefficient, subVarsDegrees), (polynomialCoefficient, _)) =>
                                val updatedCoefficient = subCoefficient - (polynomialCoefficient * factor)
                                (Utils.number2number(updatedCoefficient), subVarsDegrees)
                        }
                    )
                }
            }
            game_theory.logger.info("\n" + Utils.formatTable(newMatrix.map(_.polynomial.map(_._1).toSeq).toSeq))
            newMatrix
        }
        val solution = updatedMatrix.zipWithIndex.map { case (gameCorePolynomial, rowIndex) =>
            val coefficient = gameCorePolynomial.polynomial(rowIndex)._1
            gameCorePolynomial.polynomial.last._1  / coefficient
        }
        game_theory.logger.info("\nРешение системы уравнений методом Гаусса:\n" + solution.mkString("; "))
        solution
    }

}
