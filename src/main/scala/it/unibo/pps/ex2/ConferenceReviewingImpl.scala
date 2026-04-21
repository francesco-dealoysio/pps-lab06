package it.unibo.pps.ex2

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[ConferenceReviewing.Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: ConferenceReviewing.Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[(Int, Double)]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewingImpl = ConferenceReviewingImpl()

  enum Question:
    case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

  class  ConferenceReviewingImpl() extends ConferenceReviewing:
    var reviews: List[(Int, Map[Question, Int])] = List.empty[(Int, Map[Question, Int])]

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      require(scores.size == Question.values.length)
      reviews = reviews ::: List((article, scores))

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, `final`: Int): Unit =
      val map: Map[Question, Int] = Map(
        Question.RELEVANCE -> relevance,
        Question.SIGNIFICANCE -> significance,
        Question.CONFIDENCE -> confidence,
        Question.FINAL -> `final`
      )
      reviews = reviews ::: List((article, map))

    override def orderedScores(article: Int, question: ConferenceReviewing.Question): List[Int] =
      reviews.filter((a, _) => a == article).map((_, score) => score(question)).sorted

    override def averageFinalScore(article: Int): Double =
      val scores = reviews.filter((a, _) => a == article).map((_, score) => score(Question.FINAL))
      scores.map(_.toDouble).sum / scores.size

    override def acceptedArticles(): Set[Int] =
      reviews.map((a, _) => a).distinct.filter(a => accepted(a)).toSet

    override def sortedAcceptedArticles(): List[(Int, Double)] =
      acceptedArticles().toList.map(a => (a, averageFinalScore(a))).sortBy(a => a._2)

    override def averageWeightedFinalScoreMap(): Map[Int, Double] =
      reviews.map((a, _) => (a, averageWeightedFinalScore(a))).toMap

    private def averageWeightedFinalScore(article: Int): Double =
      var score = reviews.filter((a, _) => a == article).map((_, score) => score(Question.FINAL) * score(Question.CONFIDENCE) / 10.0)
      score.map(_.toDouble).sum / score.size

    private def accepted(article: Int): Boolean =
      averageFinalScore(article) > 5.0 &&
        reviews.filter((a, _) => a == article).map((_, s) => s(Question.RELEVANCE)).filter(r => r >= 8).length > 0

@main def tryConferenceReviewingImpl: Unit =
  println("tested in test/scala/it.unibo.pps/ex2/ConferenceReviewingTest.scala")