package game_theory.utils

import game_theory.MSA
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.ss.usermodel.{DataFormatter, Row}
import org.apache.poi.xssf.usermodel.XSSFWorkbook

import java.io.{File, FileInputStream, IOException}
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import java.nio.file.attribute.BasicFileAttributes
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Try

object Utils {

    // -----------------------------------------------------------------------------------------------------------------
    // Методы работы с числами
    // -----------------------------------------------------------------------------------------------------------------

    private val zeroEpsilon = 0.000001

    def roundWithScale(decimal: Double, scale: Int): Double =
        BigDecimal(decimal).setScale(scale, BigDecimal.RoundingMode.HALF_UP).toDouble

    def isNumberZero(number: Double): Boolean = math.abs(number) <= zeroEpsilon

    def number2number(number: Double) = if (isNumberZero(number)) 0.0 else number

    def convertExpressionToNumber(expression: String): Double = {
        val expressionWithoutWhitespacesAndCommas = expression.replaceAll("\\s", "").replaceAll(",", ".")
        val divisionPattern = "^([\\-0-9]*)[\\/:]([\\-0-9]*)$".r
        divisionPattern.findFirstMatchIn(expressionWithoutWhitespacesAndCommas).map { matchResult =>
            val leftNumber = matchResult.group(1).toDouble
            val rightNumber = matchResult.group(2).toDouble
            if (rightNumber == 0) throw new Exception("На ноль делить нельзя")
            roundWithScale(leftNumber / rightNumber, scale = 2)
        }.getOrElse(roundWithScale(expressionWithoutWhitespacesAndCommas.toDouble, scale = 2))
    }

    def getId: String = java.util.UUID.randomUUID.toString

    // -----------------------------------------------------------------------------------------------------------------
    // Методы работы с файлами
    // -----------------------------------------------------------------------------------------------------------------

    def deleteFileOrDir(paths: Seq[String]): Unit = {
        paths.foreach(deleteFileOrDir)
    }

    def deleteFileOrDir(path: String): Unit = {
        if (new File(path).exists)
            deleteFileOrDir(Paths.get(path))
    }

    def deleteFileOrDir(root: Path, deleteRoot: Boolean = true): Unit =
        Files.walkFileTree(root, new SimpleFileVisitor[Path] {
            override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
                Files.delete(file)
                FileVisitResult.CONTINUE
            }

            override def postVisitDirectory(dir: Path, exception: IOException): FileVisitResult = {
                if (deleteRoot) Files.delete(dir)
                FileVisitResult.CONTINUE
            }
        })

    def readXls(xlsFilePath: String, sheetIndex: Int = 0): Array[Array[String]] = {
        val fileName = new File(xlsFilePath).getName
        val table = new ArrayBuffer[Array[String]]

        val workbook = """(\.xls|\.xlsx)$""".r.findFirstIn(fileName.toLowerCase).getOrElse("") match {
            case ".xls" =>
                new HSSFWorkbook(new FileInputStream(new File(xlsFilePath)))
            case _ =>
                new XSSFWorkbook(new FileInputStream(new File(xlsFilePath)))
        }
        val sheet = workbook.getSheetAt(sheetIndex)

        val rowIterator = sheet.rowIterator
        val cellsCount = {
            val rowIteratorForCount = sheet.rowIterator
            if (rowIteratorForCount.hasNext) rowIteratorForCount.next.getLastCellNum.toInt else 0
        }

        while (rowIterator.hasNext) {
            val row = rowIterator.next
            val cellsValues = for (cellIndex <- 0 until cellsCount) yield {
                val cell = row.getCell(cellIndex, Row.MissingCellPolicy.CREATE_NULL_AS_BLANK)
                val cellValue = new DataFormatter().formatCellValue(cell).replaceAll("\\s", "")
                if (cellValue.isEmpty) None else Some(cellValue)
            }
            if (cellsValues.flatten.nonEmpty) {
                table.append(cellsValues.map(cellValue =>cellValue.getOrElse("")).toArray)
            }
        }
        table.toArray
    }


    // -----------------------------------------------------------------------------------------------------------------
    // Методы работы с матрицами. Антагонистические игры
    // -----------------------------------------------------------------------------------------------------------------

        // -------------------------------------------------------------------------------------------------------------
        // * Симплекс
        // -------------------------------------------------------------------------------------------------------------

    // Поиск разрешающей строки по разрешающему столбцу
    private def getPermissiveRowIndex(matrix: Matrix, permissiveColumnIndex: Int): Option[Int] = Try {
        val matrixTransposed = matrix.transpose
        val freeColumnValues = matrixTransposed.matrix.head.init
        val permissiveColumnValues = matrixTransposed.matrix(permissiveColumnIndex).init
        val permissiveRowIndex = freeColumnValues.zip(permissiveColumnValues)
            .zipWithIndex
            .filter(x => !isNumberZero(x._1._2))
            .map {
                case ((freeColumnValue, permissiveColumnValue), rowIndex) =>
                    (freeColumnValue / permissiveColumnValue, rowIndex)
            }.filter(_._1 >= 0.0).minBy(_._1)._2
        Some(permissiveRowIndex)
    }.recover {
        case exception: Exception =>
            game_theory.logger.error(exception.getMessage)
            None
    }.get

    // Замена базиса
    private def changeBasicVariables[T: ClassTag](basicVariables: Array[T],
                                     variables: Array[T],
                                     permissiveRowIndex: Int,
                                     permissiveColumnIndex: Int): (Array[T], Array[T]) = {
        (
            basicVariables.patch(permissiveRowIndex, Array[T](variables(permissiveColumnIndex - 1)), 1),
            variables.patch(permissiveColumnIndex - 1, Array[T](basicVariables(permissiveRowIndex)), 1)
        )
    }

    // Жордановы исключения
    private def computeJordanExceptions(matrix: Matrix,
                                        permissiveRowIndex: Int,
                                        permissiveColumnIndex: Int): Matrix = {
        val matrixCopy = matrix
        Matrix(matrixCopy.matrix.zipWithIndex.map { case (row, rowIndex) =>
            row.zipWithIndex.map { case (value, columnIndex) =>
                if (rowIndex == permissiveRowIndex && columnIndex == permissiveColumnIndex) {
                    roundWithScale(1 / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex), 5)
                } else if (rowIndex == permissiveRowIndex) {
                    roundWithScale(value / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex), 5)
                } else if (columnIndex == permissiveColumnIndex) {
                    roundWithScale(-value / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex), 5)
                } else {
                    roundWithScale(value - ((row(permissiveColumnIndex) * matrix.matrix(permissiveRowIndex)(columnIndex)) / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex)), 5)
                }
            }
        })
    }

    // Поиск опорного решения
    @tailrec
    private def findReferenceSolution[T: ClassTag](matrix: Matrix,
                                      basicVariables: Array[T],
                                      variables: Array[T]): Option[(Matrix, Array[T], Array[T])] = {
        val matrixTransposed = matrix.transpose
        matrixTransposed.matrix.head.zipWithIndex.init.find(_._1 < 0.0) match {
            case Some((_, freeRowIndex)) =>
                matrix.matrix(freeRowIndex).zipWithIndex.tail.find(_._1 < 0.0) match {
                    case Some((_, permissiveColumnIndex)) =>
                        getPermissiveRowIndex(matrix, permissiveColumnIndex) match {
                            case Some(permissiveRowIndex) =>
                                val (newBasicVariables, newVariables) = changeBasicVariables(basicVariables, variables, permissiveRowIndex, permissiveColumnIndex)
                                val newMatrix = computeJordanExceptions(matrix, permissiveRowIndex, permissiveColumnIndex)
                                findReferenceSolution(newMatrix, newBasicVariables, newVariables)
                            case _ =>
                                None
                        }
                    case _ =>
                        None
                }
            case _ =>
                Some(matrix, basicVariables, variables)
        }
    }

    // Поиск оптимального решения
    @tailrec
    private def findOptimalSolution[T: ClassTag](matrix: Matrix,
                                    basicVariables: Array[T],
                                    variables: Array[T]): Option[(Matrix, Array[T], Array[T])] = {
        matrix.matrix.last.zipWithIndex.tail.find(_._1 > 0) match {
            case Some((_, permissiveColumnIndex)) =>
                getPermissiveRowIndex(matrix, permissiveColumnIndex) match {
                    case Some(permissiveRowIndex) =>
                        val (newBasicVariables, newVariables) = changeBasicVariables(basicVariables, variables, permissiveRowIndex, permissiveColumnIndex)
                        val newMatrix = computeJordanExceptions(matrix, permissiveRowIndex, permissiveColumnIndex)
                        findOptimalSolution(newMatrix, newBasicVariables, newVariables)
                    case _ =>
                        None
                }
            case _ =>
                Some(matrix, basicVariables, variables)
        }
    }

    def simplex[T: ClassTag](matrix: Matrix,
                basicVariables: Array[T],
                variables: Array[T]): Option[(Matrix, Array[T], Array[T])] = {
        findReferenceSolution(matrix, basicVariables, variables) flatMap {
            case (matrixReference, basicVariablesReference, variablesReference) =>
                findOptimalSolution(matrixReference, basicVariablesReference, variablesReference)
        }
    }

        // -------------------------------------------------------------------------------------------------------------
        // * Минимакс
        // -------------------------------------------------------------------------------------------------------------

    def minmaxSolution(matrix: Matrix): Option[(Array[Double], Array[Double])] = {
        val (max, maxIndex) = matrix.matrix.zipWithIndex.map { case (row, index) => (row.min, index) }.maxBy(_._1)
        val (min, minIndex) = matrix.transpose.matrix.zipWithIndex.map { case (column, index) => (column.max, index) }.minBy(_._1)
        if (max == min) {
            Some((
                Array.fill(matrix.getMatrixRowsCount)(0.0).patch(maxIndex, Array(1.0), 1),
                Array.fill(matrix.getMatrixColumnsCount)(0.0).patch(minIndex, Array(1.0), 1)
            ))
        } else {
            None
        }
    }

    def minmaxCost(matrix: Matrix): Option[(Int, Int, Double)] = {
        val (max, maxIndex) = matrix.matrix.zipWithIndex.map { case (row, index) => (row.min, index) }.maxBy(_._1)
        val (min, minIndex) = matrix.transpose.matrix.zipWithIndex.map { case (column, index) => (column.max, index) }.minBy(_._1)
        if (max == min) {
            Some((maxIndex, minIndex, matrix.matrix(maxIndex)(minIndex)))
        } else {
            None
        }
    }

        // -------------------------------------------------------------------------------------------------------------
        // * Аналитические методы
        // -------------------------------------------------------------------------------------------------------------

            // ---------------------------------------------------------------------------------------------------------
            // ** Метод обратных матриц
            // ---------------------------------------------------------------------------------------------------------

    def analyticalInverseMatrixMethod(matrixStrategy: Matrix): Either[String, Map[String, Any]] = Try {
        game_theory.logger.info("\n" + "*" * 10 + " INVERSE MATRIX METHOD " + "*" * 10 + "\n")
        val unitMatrix = Matrix(Array(Array.fill(matrixStrategy.getMatrixRowsCount)(1.0)))
        game_theory.logger.info("\nunitMatrix:\n" + formatTable(unitMatrix.matrix.map(_.toSeq).toSeq) + "\n")
        val unitMatrixTransposed = unitMatrix.transpose
        game_theory.logger.info("\nunitMatrixTransposed:\n" + formatTable(unitMatrixTransposed.matrix.map(_.toSeq).toSeq) + "\n")
        val matrixStrategyInverse = matrixStrategy.inverse
        game_theory.logger.info("\nmatrixStrategy:\n" + formatTable(matrixStrategy.matrix.map(_.toSeq).toSeq) + "\n")
        game_theory.logger.info("\nmatrixStrategyInverse:\n" + formatTable(matrixStrategyInverse.matrix.map(_.toSeq).toSeq) + "\n")
        val gameCost = (unitMatrix * matrixStrategyInverse * unitMatrixTransposed).inverse.matrix.head.head
        game_theory.logger.info("\ngameCost= 1 / (unitMatrix * matrixStrategyInverse * unitMatrixTransposed): " + gameCost + "\n")
        val firstGamerStrategyNumerator = unitMatrix * matrixStrategyInverse
        val firstGamerStrategy = firstGamerStrategyNumerator.copy(firstGamerStrategyNumerator.matrix.map(_.map(_ * gameCost)))
        game_theory.logger.info("\nfirstGamerStrategy = (unitMatrix * matrixStrategyInverse) / (unitMatrix * matrixStrategyInverse * unitMatrixTransposed):\n" + formatTable(firstGamerStrategy.matrix.map(_.toSeq).toSeq) + "\n")
        val secondGamerStrategyNumerator = matrixStrategyInverse * unitMatrixTransposed
        val secondGamerStrategy = secondGamerStrategyNumerator.copy(secondGamerStrategyNumerator.matrix.map(_.map(_ * gameCost)))
        game_theory.logger.info("\nsecondGamerStrategy = (matrixStrategyInverse * unitMatrixTransposed) / (unitMatrix * matrixStrategyInverse * unitMatrixTransposed):\n" + formatTable(secondGamerStrategy.matrix.map(_.toSeq).toSeq) + "\n")

        Right(Map(
            "firstGamerStrategy" -> firstGamerStrategy.matrix.flatten.map(matrixElement => roundWithScale(matrixElement, scale = 2)),
            "secondGamerStrategy" -> secondGamerStrategy.matrix.flatten.map(matrixElement => roundWithScale(matrixElement, scale = 2)),
            "gameCost" -> roundWithScale(gameCost, scale = 2)
        ))
    }.recover {
        case exception: Exception =>
            val errorMessage = exception.getMessage
            game_theory.logger.error("Inverse Matrix Method error: " + errorMessage)
            Left(errorMessage)
    }.get

            // ---------------------------------------------------------------------------------------------------------
            // ** Аналитический метод решения выпукло-вогнутых игр
            // ---------------------------------------------------------------------------------------------------------

    def analyticalConvexConcaveGamesMethod(matrixStrategyKernel: GameCorePolynomial): Either[String, Map[String, Double]] = {
        game_theory.logger.info("\n" + "*" * 10 + " ANALYTICAL CONVEX CONCAVE GAMES METHOD " + "*" * 10 + "\n")
        val firstDerivativeX = matrixStrategyKernel.derivative(0)
        val firstDerivativeY = matrixStrategyKernel.derivative(1)
        val secondDerivativeX = firstDerivativeX.derivative(0).polynomial.head._1
        val secondDerivativeY = firstDerivativeY.derivative(1).polynomial.head._1
        if (secondDerivativeX < 0.0 && secondDerivativeY > 0.0) {
            val Array(solutionX, solutionY) = GameCorePolynomial.solve(Array(firstDerivativeX, firstDerivativeY))
                .map(solution => roundWithScale(solution, scale = 3))
            if (solutionX < 0.0 || solutionY < 0.0) {
                Left(s"x = $solutionX, y = $solutionY; Игра не имеет оптимальных стратегий")
            } else {
                val saddlePoint = roundWithScale(matrixStrategyKernel.compute(Array(solutionX, solutionY)), scale = 3)
                Right(Map(
                    "solutionX" -> solutionX,
                    "solutionY" -> solutionY,
                    "saddlePoint" -> saddlePoint
                ))
            }
        } else {
            Left("Игра не является выпукло-вогнутой")
        }
    }

        // -------------------------------------------------------------------------------------------------------------
        // * Численные методы
        // -------------------------------------------------------------------------------------------------------------

            // ---------------------------------------------------------------------------------------------------------
            // ** Браун - Робинсон
            // ---------------------------------------------------------------------------------------------------------

    private def findOptimalStrategies(firstGamerWins: Array[Double], secondGamerLosses: Array[Double]): (Int, Int) = {
        if (firstGamerWins.isEmpty || secondGamerLosses.isEmpty) {
            (0, 0)
        } else {
            val firstMaxWinAndStrategy = firstGamerWins.zipWithIndex.maxBy(_._1)
            val firstOptimalStrategy = firstMaxWinAndStrategy._2
            val secondMinLossAndStrategy = secondGamerLosses.zipWithIndex.minBy(_._1)
            val secondOptimalStrategy = secondMinLossAndStrategy._2
            val firstGamerMaxWins = firstGamerWins.zipWithIndex.filter(_._1 == firstMaxWinAndStrategy._1)
            val secondGamerMaxLosses = secondGamerLosses.zipWithIndex.filter(_._1 == secondMinLossAndStrategy._1)
            (
                if (firstGamerMaxWins.length > 1) {
                    firstGamerMaxWins(scala.util.Random.nextInt(firstGamerMaxWins.length))._2
                } else {
                    firstOptimalStrategy
                },
                if (secondGamerMaxLosses.length > 1) {
                    secondGamerMaxLosses(scala.util.Random.nextInt(secondGamerMaxLosses.length))._2
                } else {
                    secondOptimalStrategy
                }
            )
        }
    }

    private def getGameCostUp(firstGamerWins: Array[Double], iterationNumber: Int): Double = {
        firstGamerWins.max / iterationNumber
    }

    private def getGameCostLoss(secondGamerLosses: Array[Double], iterationNumber: Int): Double = {
        secondGamerLosses.min / iterationNumber
    }

    def brownRobinson(matrixStrategy: Matrix): Either[String, Map[String, Any]] = Try {
        game_theory.logger.info("\n" + "*"*10 + " BROWN-ROBINSON METHOD " + "*"*10 + "\n")
        var brownRobinsonMatrixAlgorithm = List.empty[MSA]

        var iterationNumber = 0

        var firstGamerStrategiesChoices = Array.fill(matrixStrategy.getMatrixRowsCount)(0)
        var secondGamerStrategiesChoices = Array.fill(matrixStrategy.getMatrixColumnsCount)(0)

        var firstGamerWins = Array.empty[Double]
        var secondGamerLosses = Array.empty[Double]

        var gameCostsAveragesUp = List.empty[Double]
        var gamesCostsAveragesLosses = List.empty[Double]

        var epsilon: Option[Double] = None

        while (epsilon.isEmpty || epsilon.get > 0.1) {
            iterationNumber += 1

            val (firstGamerOptimalStrategy, secondGamerOptimalStrategy) = findOptimalStrategies(firstGamerWins, secondGamerLosses)

            firstGamerStrategiesChoices = firstGamerStrategiesChoices.updated(
                firstGamerOptimalStrategy,
                firstGamerStrategiesChoices(firstGamerOptimalStrategy) + 1
            )
            secondGamerStrategiesChoices = secondGamerStrategiesChoices.updated(
                secondGamerOptimalStrategy,
                secondGamerStrategiesChoices(secondGamerOptimalStrategy) + 1
            )

            firstGamerWins = firstGamerWins
                .zipAll(matrixStrategy.matrix.transpose.apply(secondGamerOptimalStrategy), 0.0, 0.0)
                .map { case (oldWin, newWin) => oldWin + newWin }
            secondGamerLosses = secondGamerLosses
                .zipAll(matrixStrategy.matrix(firstGamerOptimalStrategy), 0.0, 0.0)
                .map { case (oldLoss, newLoss) => oldLoss + newLoss }


            val currentGameCostUp = getGameCostUp(firstGamerWins, iterationNumber)
            gameCostsAveragesUp = gameCostsAveragesUp :+ currentGameCostUp
            val currentGameCostLoss = getGameCostLoss(secondGamerLosses, iterationNumber)
            gamesCostsAveragesLosses = gamesCostsAveragesLosses :+ currentGameCostLoss

            epsilon = Some(gameCostsAveragesUp.min - gamesCostsAveragesLosses.max)

            brownRobinsonMatrixAlgorithm = brownRobinsonMatrixAlgorithm :+ Map(
                "iterationNumber" -> iterationNumber,
                "firstOptimalStrategy" -> (firstGamerOptimalStrategy + 1),
                "secondOptimalStrategy" -> (secondGamerOptimalStrategy + 1),
                "firstGamerWins" -> firstGamerWins,
                "secondGamerLosses" -> secondGamerLosses,
                "gameCostAverageUp" -> roundWithScale(currentGameCostUp, scale = 2),
                "gameCostAverageLoss" -> roundWithScale(currentGameCostLoss, scale = 2),
                "epsilon" -> epsilon.map(epsilonValue => roundWithScale(epsilonValue, scale = 2))
            )
        }
        val gameCostMaxLoss = roundWithScale(gamesCostsAveragesLosses.max, scale = 2)
        val gameCostMinWin = roundWithScale(gameCostsAveragesUp.min, scale = 2)
        game_theory.logger.info("\nfirstGamerStrategy: (" + firstGamerStrategiesChoices.map(choice => choice.toDouble / iterationNumber).mkString(",") + ")\n")
        game_theory.logger.info("\nsecondGamerStrategy: (" + secondGamerStrategiesChoices.map(choice => choice.toDouble / iterationNumber).mkString(",") + ")\n")
        game_theory.logger.info("\ngameCost: " + (gameCostMaxLoss + gameCostMinWin) / 2 + "\n")
        game_theory.logger.info("\nepsilon: " + epsilon.map(epsilonValue => epsilonValue).getOrElse("error") + ")\n")
        Right(Map(
            "brownRobinsonMatrixAlgorithm" -> brownRobinsonMatrixAlgorithm,
            "firstGamerStrategy" -> firstGamerStrategiesChoices.map(choice => roundWithScale(choice.toDouble / iterationNumber, scale = 2)),
            "secondGamerStrategy" -> secondGamerStrategiesChoices.map(choice => roundWithScale(choice.toDouble / iterationNumber, scale = 2)),
            "gameCost" -> List(gameCostMaxLoss, gameCostMinWin),
            "gameCostAverage" -> roundWithScale((gameCostMaxLoss + gameCostMinWin) / 2, scale = 2),
            "epsilon" -> epsilon.map(epsilonValue => roundWithScale(epsilonValue, scale = 2))
        ))
    }.recover {
        case exception: Exception =>
            val errorMessage = exception.getMessage
            game_theory.logger.error("Brown-Robinson Method error: " + errorMessage)
            Left(errorMessage)
    }.get

            // ---------------------------------------------------------------------------------------------------------
            // ** Численный метод решения выпукло-вогнутых игр
            // ---------------------------------------------------------------------------------------------------------

    private val differenceEpsilon = 0.03
    private val lastIterationsCountLimit = 5

    @tailrec
    def numericalConvexConcaveGamesMethod(matrixStrategyKernel: GameCorePolynomial,
                                          partitionParameter: Int = 1,
                                          lastIterationsCountWithMinDifference: Int = 0,
                                          previousGameCostOp: Option[Double] = None,
                                          solutions: Array[MSA] = Array()): Array[MSA] = {
        if (lastIterationsCountWithMinDifference == lastIterationsCountLimit) {
            solutions
        } else {
            val dimensions = partitionParameter + 1
            val approximation = Matrix.fill(dimensions, dimensions, 0.0).matrix.zipWithIndex.map { case (row, rowIndex) =>
                row.zipWithIndex.map { case (_, columnIndex) =>
                    roundWithScale(
                        matrixStrategyKernel.compute(Array(rowIndex.toDouble / partitionParameter, columnIndex.toDouble / partitionParameter)),
                        scale = 3
                    )
                }
            }
            game_theory.logger.info(s"\nN=$partitionParameter\n${formatTable(approximation.map(_.toSeq).toSeq)}")
            val (solutionX, solutionY, gameCost, comment) = minmaxCost(Matrix(approximation)).map {
                case (rowIndex, columnIndex, cost) =>
                    (
                        rowIndex.toDouble / partitionParameter,
                        columnIndex.toDouble / partitionParameter,
                        cost,
                        "Есть седловая точка"
                    )
            }.getOrElse {
                brownRobinson(Matrix(approximation)).map { brownRobinsonResult =>
                    val brownRobinsonMatrixAlgorithm = brownRobinsonResult("brownRobinsonMatrixAlgorithm").asInstanceOf[List[MSA]]
                    val rowIndex = brownRobinsonMatrixAlgorithm.last("firstOptimalStrategy").toString.toDouble - 1.0
                    val columnIndex = brownRobinsonMatrixAlgorithm.last("secondOptimalStrategy").toString.toDouble - 1.0
                    (
                        rowIndex / partitionParameter,
                        columnIndex / partitionParameter,
                        brownRobinsonResult("gameCostAverage").toString.toDouble,
                        "Седловой точки нет, решение методом Брауна - Робинсон"
                    )
                }.getOrElse(throw new Exception("Нет решений"))
            }
            game_theory.logger.info(s"\n$comment\nx=$solutionX; y=$solutionY; H=$gameCost")
            val isMinDifference = previousGameCostOp.map { previousGameCost =>
                math.abs(previousGameCost - gameCost) <= differenceEpsilon
            }
            numericalConvexConcaveGamesMethod(
                matrixStrategyKernel,
                partitionParameter + 1,
                if (isMinDifference.contains(true)) lastIterationsCountWithMinDifference + 1 else 0,
                Some(gameCost),
                solutions :+ Map(
                    "partitionParameter" -> partitionParameter,
                    "approximation" -> approximation,
                    "solutionX" -> roundWithScale(solutionX, scale = 3),
                    "solutionY" -> roundWithScale(solutionY, scale = 3),
                    "gameCost" -> roundWithScale(gameCost, scale = 3),
                    "comment" -> comment
                )
            )
        }
    }

    // -----------------------------------------------------------------------------------------------------------------
    // Методы работы с матрицами. Неантагонистические игры
    // -----------------------------------------------------------------------------------------------------------------

        // -------------------------------------------------------------------------------------------------------------
        // * Равновесие по Нэшу
        // -------------------------------------------------------------------------------------------------------------

    private def findNashEquilibriumInMixedStrategies(matricesStrategy: Array[Matrix]): Either[String, MSA] = {
        var exceptions = matricesStrategy.zipWithIndex.foldLeft(Array.empty[String]) { case (errors, (matrix, matrixIndex)) =>
            if (!matrix.IsNonDegenerateMatrix) errors :+ s"Матрица $matrixIndex вырожденная. Нет смешанной ситуации равновесия"
            else errors
        }
        if (exceptions.nonEmpty) {
            game_theory.logger.error("Find Nash Equilibrium In Clear Strategies error: " + exceptions.mkString("\n"))
            Left(exceptions.mkString("\n"))
        } else {
            val nashEquilibriumInMixedStrategies = matricesStrategy.map(analyticalInverseMatrixMethod)
                .flatMap {
                    case Left(errMsg) =>
                        exceptions = exceptions :+ errMsg
                        None
                    case Right(analyticalInverseMatrixMethodResult) =>
                        Some(analyticalInverseMatrixMethodResult)
                }
            val gameCosts = nashEquilibriumInMixedStrategies.map(_("gameCost"))
            val strategies = nashEquilibriumInMixedStrategies.map(_("firstGamerStrategy")).reverse
            val result =
                if (strategies.map(_.asInstanceOf[Array[Double]]).exists(_.exists(_ < 0.0))) {
                    Map.empty[String, Any]
                } else {
                    val nashEquilibriumInMixed = strategies.zip(gameCosts).map { case (gamerStrategy, gameCost) =>
                        Map("gamerStrategy" -> gamerStrategy, "gameCost" -> gameCost)
                    }
                    Map("nashEquilibriumInMixedStrategies" -> nashEquilibriumInMixed)
                }
            if (exceptions.nonEmpty) {
                game_theory.logger.error("Find Nash Equilibrium In Clear Strategies error: " + exceptions.mkString("\n"))
                Left(exceptions.mkString("\n"))
            } else {
                Right(result)
            }
        }
    }

     private def findNashEquilibriumInClearStrategies(matricesStrategyAndIsTransposed: Array[(Matrix, Boolean)]): Either[String, MSA] =
         Try {
             val nashEquilibriumInClear = matricesStrategyAndIsTransposed
                 .flatMap { case (matrixStrategy, isTransposed) =>
                     matrixStrategy.matrix.zipWithIndex.flatMap { case (row, rowIndex) =>
                         row.zipWithIndex.foldLeft(Array.empty[(Double, Int, Int)]) { case (maxElements, (element, columnIndex)) =>
                             val (sourceRowIndex, sourceColumnIndex) = if (!isTransposed) (rowIndex, columnIndex) else (columnIndex, rowIndex)
                             maxElements.headOption match {
                                 case Some(maxElem) =>
                                     if (element > maxElem._1) Array((element, sourceRowIndex, sourceColumnIndex))
                                     else if (element == maxElem._1) maxElements :+ (element, sourceRowIndex, sourceColumnIndex)
                                     else maxElements
                                 case _ =>
                                     Array((element, sourceRowIndex, sourceColumnIndex))
                             }
                         }
                     }
                 }
                 .groupBy { case (_, rowIndex, columnIndex) => (rowIndex, columnIndex) }
                 .filter(_._2.length > 1).values.flatten
                 .map { elementIndices =>
                     Map("value" -> elementIndices._1, "rowIndex" -> elementIndices._2, "columnIndex" -> elementIndices._3).asInstanceOf[MSA]
                 }.toArray
             Right(Map("nashEquilibriumInClearStrategies" -> nashEquilibriumInClear))
         }.recover {
             case exception: Exception =>
                 val errorMessage = exception.getMessage
                 game_theory.logger.error("Find Nash Equilibrium In Clear Strategies error: " + errorMessage)
                 Left(errorMessage)
         }.get

    private def hasStrictlyDominantStrategy(matricesStrategy: Matrix): Boolean = {
        matricesStrategy.matrix.map(row => row.zipWithIndex.maxBy(_._1)).map(_._2).toSet.size == 1
    }

    def findNashEquilibrium(matricesStrategyAndIsTransposed: Array[(Matrix, Boolean)]): Either[String, MSA] = {
        if (matricesStrategyAndIsTransposed.head._1.getMatrixRowsCount == 2 &&
            matricesStrategyAndIsTransposed.head._1.getMatrixColumnsCount == 2) {
            if (matricesStrategyAndIsTransposed.exists { case (matrixStrategy, _) => hasStrictlyDominantStrategy(matrixStrategy) }) {
                findNashEquilibriumInClearStrategies(matricesStrategyAndIsTransposed).flatMap { nashEquilibriumInClearStrategies =>
                    findNashEquilibriumInMixedStrategies(matricesStrategyAndIsTransposed.map(_._1)).map { nashEquilibriumInMixedStrategies =>
                        nashEquilibriumInClearStrategies ++ nashEquilibriumInMixedStrategies
                    }
                }
            } else {
                findNashEquilibriumInClearStrategies(matricesStrategyAndIsTransposed).flatMap { nashEquilibriumInClearStrategies =>
                    val nashEquilibriumInClearStrategiesResult = nashEquilibriumInClearStrategies("nashEquilibriumInClearStrategies").asInstanceOf[Array[MSA]]
                    if (nashEquilibriumInClearStrategiesResult.nonEmpty) {
                        if (nashEquilibriumInClearStrategiesResult.length >= 4) {
                            findNashEquilibriumInMixedStrategies(matricesStrategyAndIsTransposed.map(_._1)).map { nashEquilibriumInMixedStrategies =>
                                nashEquilibriumInClearStrategies ++ nashEquilibriumInMixedStrategies
                            }
                        } else {
                            Right(nashEquilibriumInClearStrategies)
                        }
                    } else {
                        findNashEquilibriumInMixedStrategies(matricesStrategyAndIsTransposed.map(_._1))
                    }
                }
            }
        } else {
            findNashEquilibriumInClearStrategies(matricesStrategyAndIsTransposed)
        }
    }

        // -------------------------------------------------------------------------------------------------------------
        // * Оптимальность по Парето
        // -------------------------------------------------------------------------------------------------------------

    def findOptimalPareto(matricesStrategy: Array[Matrix]): Either[String, MSA] = Try {
        val matrixWithAllWins = matricesStrategy.head.matrix.zipWithIndex.map { case (row, rowIndex) =>
            row.zipWithIndex.map { case (element, columnIndex) =>
                Array(element) ++ matricesStrategy.tail.map { otherMatrixStrategy =>
                    otherMatrixStrategy.matrix(rowIndex)(columnIndex)
                }
            }
        }
        val optimalElements = matrixWithAllWins.zipWithIndex.flatMap { case (row, rowIndex) =>
            row.zipWithIndex.flatMap { case (element, columnIndex) =>
                val isOptimal = !matrixWithAllWins.zipWithIndex.exists { case (otherColumns, otherRowIndex) =>
                    otherColumns.zipWithIndex.exists { case (otherElement, otherColumnIndex) =>
                        if (rowIndex == otherRowIndex && columnIndex == otherColumnIndex) {
                            false
                        } else {
                            val elementsZipped = otherElement.zip(element)
                            if (elementsZipped.head._1 == elementsZipped.head._2) {
                                !elementsZipped.tail.exists { case (other, current) => other <= current }
                            } else if (elementsZipped.head._1 > elementsZipped.head._2) {
                                !elementsZipped.tail.exists { case (other, current) => other < current }
                            } else {
                                false
                            }
                        }
                    }
                }
                if (isOptimal) Some(Map("value" -> element, "rowIndex" -> rowIndex, "columnIndex" -> columnIndex))
                else None
            }
        }
        Right(Map("optimalPareto" -> optimalElements))
    }.recover {
        case exception: Exception =>
            val errorMessage = exception.getMessage
            game_theory.logger.error("Find Optimal Pareto error: " + errorMessage)
            Left(errorMessage)
    }.get

        // -------------------------------------------------------------------------------------------------------------
        // * Игры поиска
        // -------------------------------------------------------------------------------------------------------------

    private def planeIntersectionParameterHandler(denominator: Double, radius: Double): Double = {
        val parameter = radius / denominator
        if (parameter < 0.0) -parameter else parameter
    }

    private def planeIntersectionParameter(radius: Double, point: Point): Option[Double] = {
        (point.x - radius, point.y - radius, point.z - radius) match {
            case (0.0, 0.0, 0.0) => None
            case (denominatorX, 0.0, 0.0) => Some(planeIntersectionParameterHandler(denominatorX, radius))
            case (0.0, denominatorY, 0.0) => Some(planeIntersectionParameterHandler(denominatorY, radius))
            case (0.0, 0.0, denominatorZ) => Some(planeIntersectionParameterHandler(denominatorZ, radius))
            case (0.0, denominatorY, denominatorZ) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorY, radius),
                    planeIntersectionParameterHandler(denominatorZ, radius)
                ).min)
            case (denominatorX, 0.0, denominatorZ) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorX, radius),
                    planeIntersectionParameterHandler(denominatorZ, radius)
                ).min)
            case (denominatorX, denominatorY, 0.0) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorX, radius),
                    planeIntersectionParameterHandler(denominatorY, radius)
                ).min)
            case (denominatorX, denominatorY, denominatorZ) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorX, radius),
                    planeIntersectionParameterHandler(denominatorY, radius),
                    planeIntersectionParameterHandler(denominatorZ, radius)
                ).min)
        }
    }

    private def projectPoint2Cube(radius: Double, point: Point) = {
        planeIntersectionParameter(radius, point).map { parameter =>
            val x = (point.x - radius) * parameter + radius
            val y = (point.y - radius) * parameter + radius
            val z = (point.z - radius) * parameter + radius
            Point(x, y, z)
        }
    }

    private def generateFirstGamerStrategyOnCube(pointsCount: Int, cubeDimensions: Double, epsilon: Double): List[Point] = {
        val radius = cubeDimensions / 2.0
        val pointsIndices = (0 until pointsCount).toList
        val points = pointsIndices.map { pointIndex =>
            val theta = (2.0 * math.Pi * pointIndex) / ((1 + math.pow(5.0, 0.5)) / 2.0)
            val phi = math.acos(1.0 - (2.0 * pointIndex / pointsCount))

            val x = radius * Math.cos(theta) * Math.sin(phi) + radius
            val y = radius * Math.sin(theta) * Math.sin(phi) + radius
            val z = radius * Math.cos(phi) + radius

            Point(Utils.roundWithScale(x, 10), Utils.roundWithScale(y, 10), Utils.roundWithScale(z, 10))
        }
        points.filter(point => point.notIntersectWith(points.filter(_ != point), epsilon))
            .flatMap { point =>
                val projectedPoint = projectPoint2Cube(radius, point)
                projectedPoint.map { case Point(x, y, z) =>
                    Point(Utils.roundWithScale(x, 10), Utils.roundWithScale(y, 10), Utils.roundWithScale(z, 10))
                }
            }
    }

    def searchGame(pointsCount: Int, cubeDimensions: Double, epsilon: Double) = {
        val gamesCount = 120
        val gamesCountForOnePlane = gamesCount / 6
        val firstGamerPoints = generateFirstGamerStrategyOnCube(pointsCount, cubeDimensions, epsilon)
        val secondGamerPointsWithWins = {
            (0 until gamesCountForOnePlane).toList.map(_ => Point(0.0, cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions * scala.util.Random.nextDouble())) ++
            (0 until gamesCountForOnePlane).toList.map(_ => Point(cubeDimensions * scala.util.Random.nextDouble(), 0.0, cubeDimensions * scala.util.Random.nextDouble())) ++
            (0 until gamesCountForOnePlane).toList.map(_ => Point(cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions * scala.util.Random.nextDouble(), 0.0)) ++
            (0 until gamesCountForOnePlane).toList.map(_ => Point(cubeDimensions, cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions * scala.util.Random.nextDouble())) ++
            (0 until gamesCountForOnePlane).toList.map(_ => Point(cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions, cubeDimensions * scala.util.Random.nextDouble())) ++
            (0 until gamesCountForOnePlane).toList.map(_ => Point(cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions))
        }.map { secondPoint =>
            if (secondPoint.notIntersectWith(firstGamerPoints, epsilon)) (secondPoint, true)
            else (secondPoint, false)
        }
        val gameCost = Utils.roundWithScale(
            secondGamerPointsWithWins.count(_._2 == false).toDouble / secondGamerPointsWithWins.length,
            3
        )
        Map(
            "cubeDimensions" -> cubeDimensions,
            "firstGamerPoints" -> firstGamerPoints.map(_.toMap),
            "secondGamerPoints" -> secondGamerPointsWithWins.map(_._1.toMap),
            "gameCost" -> gameCost,
            "gamesCount" -> secondGamerPointsWithWins.length,
            "epsilon" -> epsilon
        )
    }

        // -------------------------------------------------------------------------------------------------------------
        // * Позиционные игры
        // -------------------------------------------------------------------------------------------------------------

    private var tree = Map.empty[String, TreeNode]

    def setTree(newTree: Map[String, TreeNode]) = { tree = newTree }
    def getTree = tree

    private def generateTreeHandler(
                                       root: TreeNode,
                                       lastChildInBound: Boolean,
                                       treeDepth: Int,
                                       gamersCount: Int,
                                       gamersStrategiesCount: Map[Int, Int],
                                       winMin: Int,
                                       winMax: Int,
                                       treeLevel: Int = 0
                                   ): Unit = {
        if (treeLevel != treeDepth) {
            val gamerIndex = treeLevel % gamersCount
            val strategiesCount = gamersStrategiesCount(gamerIndex)

            val leaf = treeLevel == (treeDepth - 2)
            val additionalLeavesIndices = if (treeLevel > 0) {
                val availableCountOfLeaves = if (lastChildInBound) strategiesCount - 1 else strategiesCount
                (0 until availableCountOfLeaves).map(_ => scala.util.Random.nextInt(strategiesCount + 1)).toList
            } else {
                List()
            }
            val children = if (root.leaf) {
                List()
            } else {
                (0 until strategiesCount).map { strategyIndex =>
                    val wins = if (leaf || additionalLeavesIndices.contains(strategyIndex)) {
                        (0 until gamersCount).map(_ => winMax - scala.util.Random.nextInt(winMax - winMin)).toList
                    } else {
                        List()
                    }
                    TreeNode(
                        Utils.getId,
                        List(),
                        Some(root.id),
                        if (wins.nonEmpty) List(wins) else List(),
                        treeLevel + 1,
                        if (additionalLeavesIndices.contains(strategyIndex)) true else leaf
                    )
                }.toList
            }

            val updatedRoot = root.copy(children = children.map(_.id))
            tree = tree + (updatedRoot.id -> updatedRoot)
            children.zipWithIndex.foreach { case (child, childIndex) =>
                generateTreeHandler(
                    child, childIndex == children.length - 1,
                    treeDepth, gamersCount, gamersStrategiesCount,
                    winMin, winMax, treeLevel + 1
                )
            }
        }
    }

    def generateTree(treeDepth: Int, gamersCount: Int, strategiesCount: List[Int], winMin: Int, winMax: Int): Unit = {
        tree = Map.empty[String, TreeNode]
        generateTreeHandler(
            TreeNode(Utils.getId, List(), None, List(), 0, leaf = false, root = true),
            true,
            treeDepth,
            gamersCount,
            strategiesCount.zipWithIndex.map { case (strategy, gamerIndex) => gamerIndex -> strategy }.toMap,
            winMin,
            winMax
        )
    }

    private def unBestChildren(rootId: String): Unit = {
        val root = tree.find(_._1 == rootId).getOrElse(throw new Exception(s"node with id=${rootId} not found"))._2
        val unBestedChildren = root.children.map { childId =>
            val child = tree.find(_._1 == childId).getOrElse(throw new Exception(s"node with id=${childId} not found"))._2
            child.copy(best = false)
        }
        tree = tree ++ unBestedChildren.map(child => child.id -> child).toMap
        unBestedChildren.foreach(child => unBestChildren(child.id))
    }

    def computePositionGame(root: TreeNode, gamersCount: Int): TreeNode = {
        val childrenWithWins = root.children.map { childId =>
            val child = tree.getOrElse(childId, throw new Exception(s"node with id=${childId} not found"))
            if (child.wins.isEmpty) {
                computePositionGame(child, gamersCount)
            } else {
                child
            }
        }
        val childrenWithMaxWins = childrenWithWins.groupBy { child =>
            val parentId = root.id
            val parent = tree.getOrElse(parentId, throw new Exception(s"node with id=${parentId} not found"))
            val gamerIndex = parent.level % gamersCount
            child.wins.map(win => win(gamerIndex)).max
        }.maxBy(_._1)._2
        val updatedRoot =  if (!root.root) {
            childrenWithWins.filter(child => !childrenWithMaxWins.contains(child)).foreach { child =>
                tree = tree + (child.id -> child.copy(best = false))
                unBestChildren(child.id)
            }
            root.copy(wins = childrenWithMaxWins.flatMap(_.wins))
        } else {
            childrenWithWins.foldLeft(root) { (newRoot, child) =>
                val bestWins = child.wins.groupBy(win => win.head).maxBy(_._1)._2
                val rootWithWins = newRoot.copy(wins = newRoot.wins ++ bestWins)
                val badWins = child.wins.filter(win => !bestWins.contains(win))
                val badChildren = child.children.flatMap { otherChildId =>
                    val otherChild = tree.getOrElse(otherChildId, throw new Exception(s"node with id=${otherChildId} not found"))
                    if (badWins.contains(otherChild.wins)) Some(otherChild) else None
                }
                badChildren.foreach { badChild =>
                    val unBestedRoot = badChild.copy(best = false)
                    tree = tree + (unBestedRoot.id -> unBestedRoot)
                    unBestChildren(badChild.id)
                }
                tree = tree + (child.id -> child.copy(best = true))
                rootWithWins
            }.copy(best = true)
        }
        tree = tree + (updatedRoot.id -> updatedRoot)
        tree = tree ++ childrenWithMaxWins.map( child => child.id -> child.copy(best = true)).toMap
        updatedRoot
    }

    private val availableColors = List("#fa9292", "#4d7fa0", "#dbbf79", "#d179db", "#79dbbf")
    private val availableGamers = Map(0 -> "A", 1 -> "B", 2 -> "C", 3 -> "D", 4 -> "E")

    @tailrec
    def setColorForTree(nodes: List[(TreeNode, String)], first: Boolean = true): Unit = {
        if (nodes.nonEmpty) {
            val parentsWithColor = nodes.zipWithIndex.flatMap { case ((node, color), colorIndex) =>
                val newColor = if (first) availableColors(colorIndex) else color
                val updatedNode = node.copy(color = newColor)
                tree = tree + (updatedNode.id -> updatedNode)
                tree.find(_._1 == node.parent.getOrElse("")).map(node => (node._2, newColor))
            }
            setColorForTree(parentsWithColor, false)
        }
    }

    def setGamer(gamersCount: Int): Unit = {
        tree = tree.map { case (id, node) =>
            id -> node.copy(gamer = if (node.leaf) "" else availableGamers(node.level % gamersCount))
        }
    }

    // -----------------------------------------------------------------------------------------------------------------
    // Методы работы с таблицами
    // -----------------------------------------------------------------------------------------------------------------

    def formatTable(table: Seq[Seq[Any]]): String = table match {
        case Seq() =>
            ""
        case _ =>
            val sizes = table.map(row => row.map(element =>  if (element == null) 0 else element.toString.length))
            val columnsSizes = sizes.transpose.map(size => size.max)
            val rows = table.map(row => formatRow(row, columnsSizes))
            formatRows(rowSeparator(columnsSizes), rows)
    }

    private def formatRows(rowSeparator: String, rows: Seq[String]): String = {
        (rows.map(row => List(rowSeparator, row).mkString("\n")) :+ rowSeparator).mkString("\n")
    }

    private def formatRow(row: Seq[Any], columnsSizes: Seq[Int]): String = {
        val columns = row.zip(columnsSizes).map { case (element, size) =>
            if (size == 0) "" else ("%" + size + "s").format(element)
        }
        columns.mkString("|", "|", "|")
    }

    private def rowSeparator(columnsSizes: Seq[Int]): String = columnsSizes.map("-" * _).mkString("+", "+", "+")

}
