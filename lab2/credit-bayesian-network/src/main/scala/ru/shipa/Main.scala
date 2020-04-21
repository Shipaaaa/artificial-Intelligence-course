package ru.shipa

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.{Select, Universe}
import com.cra.figaro.library.compound._

object Main {
  Universe.createNew()

  private final val TEENAGER = 'between16and17
  private final val ADULT = 'between18and59
  private final val PENSIONER = 'over60

  private final val NONE = 'none
  private final val ONE = 'one
  private final val MORE_THEN_ONE = 'moreThenOne

  private final val YES = true
  private final val NO = false

  private final val EXCELLENT = 'excellent
  private final val ACCEPTABLE = 'acceptable
  private final val UNACCEPTABLE = 'unacceptable

  private final val HIGH = 'high
  private final val MEDIUM = 'medium
  private final val LOW = 'low

  private final val PROMISING = 'promising
  private final val NOT_PROMISING = 'not_promising
  
  private val ageNode = Select(0.1 -> TEENAGER, 0.7 -> ADULT, 0.2 -> PENSIONER)
  private val hasAnotherCreditNode = Select(0.6 -> NONE, 0.3 -> ONE, 0.1 -> MORE_THEN_ONE)
  private val hasRegularIncomeNode = Select(0.79 -> YES, 0.21 -> NO)
  private val ratioOfDebtsToIncomeNode = Select(0.33 -> LOW, 0.67 -> HIGH)
  private val reliabilityOfEmployerNode = Select(0.58 -> HIGH, 0.42 -> LOW)

  private val incomeNode = createIncomeNode
  private val assetsNode = createAssetsNode
  private val creditHistoryNode = createCreditHistoryNode
  private val futureIncomeNode = createFutureIncomeNode
  private val reliabilityNode = createReliabilityNode

  private val creditResultNode = createCreditResultNode

  def main(args: Array[String]) {
    ageNode.observe(ADULT)
    hasAnotherCreditNode.observe(NONE)
    hasRegularIncomeNode.observe(YES)
    ratioOfDebtsToIncomeNode.observe(LOW)
    reliabilityOfEmployerNode.observe(HIGH)

    incomeNode.observe(HIGH)
    assetsNode.observe(HIGH)
    creditHistoryNode.observe(EXCELLENT)
    futureIncomeNode.observe(PROMISING)
    reliabilityNode.observe(YES)

    val alg = VariableElimination(creditResultNode)
    alg.start

    println("Вероятность успешного погашения кредита: " + alg.probability(creditResultNode, YES))

    alg.kill
  }

  private def createIncomeNode: RichCPD2[Boolean, Symbol, Symbol] = {
    RichCPD(hasRegularIncomeNode, hasAnotherCreditNode,
      (OneOf(YES), OneOf(NONE)) -> Select(0.38 -> HIGH, 0.33 -> MEDIUM, 0.29 -> LOW),
      (OneOf(YES), OneOf(ONE)) -> Select(0.43 -> HIGH, 0.30 -> MEDIUM, 0.27 -> LOW),
      (OneOf(YES), OneOf(MORE_THEN_ONE)) -> Select(0.47 -> HIGH, 0.28 -> MEDIUM, 0.25 -> LOW),

      (OneOf(NO), OneOf(NONE)) -> Select(0.40 -> HIGH, 0.30 -> MEDIUM, 0.30 -> LOW),
      (OneOf(NO), OneOf(ONE)) -> Select(0.57 -> HIGH, 0.24 -> MEDIUM, 0.19 -> LOW),
      (OneOf(NO), OneOf(MORE_THEN_ONE)) -> Select(0.60 -> HIGH, 0.30 -> MEDIUM, 0.10 -> LOW)
    )
  }

  private def createAssetsNode: RichCPD1[Symbol, Symbol] = {
    RichCPD(incomeNode,
      OneOf(HIGH) -> Select(0.6 -> HIGH, 0.3 -> MEDIUM, 0.1 -> LOW),
      OneOf(MEDIUM) -> Select(0.5 -> HIGH, 0.3 -> MEDIUM, 0.2 -> LOW),
      OneOf(LOW) -> Select(0.2 -> HIGH, 0.3 -> MEDIUM, 0.5 -> LOW)
    )
  }

  private def createFutureIncomeNode: RichCPD3[Symbol, Symbol, Symbol, Symbol] = {
    RichCPD(assetsNode, incomeNode, reliabilityOfEmployerNode,
      (OneOf(HIGH), OneOf(HIGH), OneOf(HIGH)) -> Select(0.9 -> PROMISING, 0.1 -> NOT_PROMISING),
      (OneOf(HIGH), OneOf(HIGH), OneOf(LOW)) -> Select(0.8 -> PROMISING, 0.2 -> NOT_PROMISING),

      (OneOf(HIGH), OneOf(MEDIUM), OneOf(HIGH)) -> Select(0.8 -> PROMISING, 0.2 -> NOT_PROMISING),
      (OneOf(HIGH), OneOf(MEDIUM), OneOf(LOW)) -> Select(0.7 -> PROMISING, 0.3 -> NOT_PROMISING),

      (OneOf(HIGH), OneOf(LOW), OneOf(HIGH)) -> Select(0.7 -> PROMISING, 0.3 -> NOT_PROMISING),
      (OneOf(HIGH), OneOf(LOW), OneOf(LOW)) -> Select(0.6 -> PROMISING, 0.4 -> NOT_PROMISING),

      (OneOf(MEDIUM), OneOf(HIGH), OneOf(HIGH)) -> Select(0.6 -> PROMISING, 0.4 -> NOT_PROMISING),
      (OneOf(MEDIUM), OneOf(HIGH), OneOf(LOW)) -> Select(0.5 -> PROMISING, 0.5 -> NOT_PROMISING),

      (OneOf(MEDIUM), OneOf(MEDIUM), OneOf(HIGH)) -> Select(0.5 -> PROMISING, 0.5 -> NOT_PROMISING),
      (OneOf(MEDIUM), OneOf(MEDIUM), OneOf(LOW)) -> Select(0.4 -> PROMISING, 0.6 -> NOT_PROMISING),

      (OneOf(MEDIUM), OneOf(LOW), OneOf(HIGH)) -> Select(0.4 -> PROMISING, 0.6 -> NOT_PROMISING),
      (OneOf(MEDIUM), OneOf(LOW), OneOf(LOW)) -> Select(0.3 -> PROMISING, 0.7 -> NOT_PROMISING),

      (OneOf(LOW), OneOf(HIGH), OneOf(HIGH)) -> Select(0.3 -> PROMISING, 0.7 -> NOT_PROMISING),
      (OneOf(LOW), OneOf(HIGH), OneOf(LOW)) -> Select(0.2 -> PROMISING, 0.8 -> NOT_PROMISING),

      (OneOf(LOW), OneOf(MEDIUM), OneOf(HIGH)) -> Select(0.2 -> PROMISING, 0.8 -> NOT_PROMISING),
      (OneOf(LOW), OneOf(MEDIUM), OneOf(LOW)) -> Select(0.1 -> PROMISING, 0.9 -> NOT_PROMISING),

      (OneOf(LOW), OneOf(LOW), OneOf(HIGH)) -> Select(0.1 -> PROMISING, 0.9 -> NOT_PROMISING),
      (OneOf(LOW), OneOf(LOW), OneOf(LOW)) -> Select(0.01 -> PROMISING, 0.99 -> NOT_PROMISING)
    )
  }

  private def createCreditHistoryNode: RichCPD2[Symbol, Symbol, Symbol] = {
    RichCPD(ageNode, ratioOfDebtsToIncomeNode,
      (OneOf(TEENAGER), OneOf(LOW)) -> Select(0.15 -> EXCELLENT, 0.25 -> ACCEPTABLE, 0.60 -> UNACCEPTABLE),
      (OneOf(TEENAGER), OneOf(HIGH)) -> Select(0.10 -> EXCELLENT, 0.20 -> ACCEPTABLE, 0.70 -> UNACCEPTABLE),

      (OneOf(ADULT), OneOf(LOW)) -> Select(0.30 -> EXCELLENT, 0.40 -> ACCEPTABLE, 0.30 -> UNACCEPTABLE),
      (OneOf(ADULT), OneOf(HIGH)) -> Select(0.20 -> EXCELLENT, 0.30 -> ACCEPTABLE, 0.50 -> UNACCEPTABLE),

      (OneOf(PENSIONER), OneOf(LOW)) -> Select(0.40 -> EXCELLENT, 0.50 -> ACCEPTABLE, 0.10 -> UNACCEPTABLE),
      (OneOf(PENSIONER), OneOf(HIGH)) -> Select(0.35 -> EXCELLENT, 0.45 -> ACCEPTABLE, 0.20 -> UNACCEPTABLE)
    )
  }

  private def createReliabilityNode: RichCPD3[Symbol, Symbol, Boolean, Boolean] = {
    RichCPD(creditHistoryNode, ageNode, hasRegularIncomeNode,
      (OneOf(EXCELLENT), OneOf(TEENAGER), OneOf(YES)) -> Select(0.69 -> YES, 0.31 -> NO),
      (OneOf(EXCELLENT), OneOf(TEENAGER), OneOf(NO)) -> Select(0.65 -> YES, 0.35 -> NO),

      (OneOf(EXCELLENT), OneOf(ADULT), OneOf(YES)) -> Select(0.8 -> YES, 0.2 -> NO),
      (OneOf(EXCELLENT), OneOf(ADULT), OneOf(NO)) -> Select(0.75 -> YES, 0.25 -> NO),

      (OneOf(EXCELLENT), OneOf(PENSIONER), OneOf(YES)) -> Select(0.9 -> YES, 0.1 -> NO),
      (OneOf(EXCELLENT), OneOf(PENSIONER), OneOf(NO)) -> Select(0.85 -> YES, 0.15 -> NO),

      (OneOf(ACCEPTABLE), OneOf(TEENAGER), OneOf(YES)) -> Select(0.4 -> YES, 0.6 -> NO),
      (OneOf(ACCEPTABLE), OneOf(TEENAGER), OneOf(NO)) -> Select(0.35 -> YES, 0.65 -> NO),

      (OneOf(ACCEPTABLE), OneOf(ADULT), OneOf(YES)) -> Select(0.51 -> YES, 0.49 -> NO),
      (OneOf(ACCEPTABLE), OneOf(ADULT), OneOf(NO)) -> Select(0.43 -> YES, 0.57 -> NO),

      (OneOf(ACCEPTABLE), OneOf(PENSIONER), OneOf(YES)) -> Select(0.6 -> YES, 0.4 -> NO),
      (OneOf(ACCEPTABLE), OneOf(PENSIONER), OneOf(NO)) -> Select(0.55 -> YES, 0.45 -> NO),

      (OneOf(UNACCEPTABLE), OneOf(TEENAGER), OneOf(YES)) -> Select(0.1 -> YES, 0.9 -> NO),
      (OneOf(UNACCEPTABLE), OneOf(TEENAGER), OneOf(NO)) -> Select(0.05 -> YES, 0.95 -> NO),

      (OneOf(UNACCEPTABLE), OneOf(ADULT), OneOf(YES)) -> Select(0.2 -> YES, 0.8 -> NO),
      (OneOf(UNACCEPTABLE), OneOf(ADULT), OneOf(NO)) -> Select(0.15 -> YES, 0.85 -> NO),

      (OneOf(UNACCEPTABLE), OneOf(PENSIONER), OneOf(YES)) -> Select(0.3 -> YES, 0.7 -> NO),
      (OneOf(UNACCEPTABLE), OneOf(PENSIONER), OneOf(NO)) -> Select(0.25 -> YES, 0.75 -> NO),
    )
  }

  private def createCreditResultNode: RichCPD3[Boolean, Symbol, Symbol, Boolean] = {
    RichCPD(reliabilityNode, futureIncomeNode, ratioOfDebtsToIncomeNode,
      (OneOf(YES), OneOf(PROMISING), OneOf(LOW)) -> Select(0.85 -> YES, 0.15 -> NO),
      (OneOf(YES), OneOf(PROMISING), OneOf(HIGH)) -> Select(0.70 -> YES, 0.30 -> NO),

      (OneOf(YES), OneOf(NOT_PROMISING), OneOf(LOW)) -> Select(0.65 -> YES, 0.35 -> NO),
      (OneOf(YES), OneOf(NOT_PROMISING), OneOf(HIGH)) -> Select(0.56 -> YES, 0.44 -> NO),

      (OneOf(NO), OneOf(PROMISING), OneOf(LOW)) -> Select(0.5 -> YES, 0.5 -> NO),
      (OneOf(NO), OneOf(PROMISING), OneOf(HIGH)) -> Select(0.44 -> YES, 0.56 -> NO),

      (OneOf(NO), OneOf(NOT_PROMISING), OneOf(LOW)) -> Select(0.37 -> YES, 0.63 -> NO),
      (OneOf(NO), OneOf(NOT_PROMISING), OneOf(HIGH)) -> Select(0.28 -> YES, 0.72 -> NO)
    )
  }

}
