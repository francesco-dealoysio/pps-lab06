package it.unibo.pps.ex2

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[ConferenceReviewing.Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: ConferenceReviewing.Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[(Int, Double)]
  /*
    def averageWeightedFinalScoreMap(): Map[Int, Double]
  */
  def accepted(article: Int): Boolean // MUST BE PRIVATE (WILL BE REMOVED)

object ConferenceReviewing:
  def apply(): ConferenceReviewingImpl = ConferenceReviewingImpl()

  enum Question:
    case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

  class  ConferenceReviewingImpl() extends ConferenceReviewing:
    var reviews: List[(Int, Map[Question, Int])] = List.empty[(Int, Map[Question, Int])]

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      require(scores.size == Question.values.length) // if false throws an IllegalArgumentException
      reviews = reviews ::: List((article, scores))

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      val map: Map[Question, Int] = Map(
        Question.RELEVANCE -> relevance,
        Question.SIGNIFICANCE -> significance,
        Question.CONFIDENCE -> confidence,
        Question.FINAL -> fin
      )
      reviews = reviews ::: List((article, map)) // tail concatenate
      //reviews = List((article, map)) ::: reviews // head concatenate
      //reviews = (article, map) :: reviews // prepend new element

    override def orderedScores(article: Int, question: ConferenceReviewing.Question): List[Int] =
      reviews.filter((a, _) => a == article).map((_, score) => score(question)).sorted

    override def averageFinalScore(article: Int): Double =
      val scores = reviews.filter((a, _) => a == article).map((_, score) => score(Question.FINAL))
      scores.map(_.toDouble).sum / scores.size

    override def acceptedArticles(): Set[Int] =
      reviews.map((a, _) => a).distinct.filter(a => accepted(a)).toSet

    override def sortedAcceptedArticles(): List[(Int, Double)] =
      acceptedArticles().toList.map(a => (a , averageFinalScore(a))).sortBy(a => a._2)

    // MUST BE PRIVATE (REPLACE OVERRIDE WITH PRIVATE)
    override def accepted(article: Int): Boolean =
//     val req1: Boolean = averageFinalScore(article) > 5.0
      //val relevances = reviews.filter((a, _) => a == article).map((_, score) => score(Question.RELEVANCE))
      //val req2: Boolean = relevances.filter(r => r >= 8).length > 0
//     req1 && reviews.filter((a, _) => a == article).map((_, score) => score(Question.RELEVANCE)).filter(r => r >= 8).length > 0
      //req1 && req2
      averageFinalScore(article) > 5.0 &&
        reviews.filter((a, _) => a == article).map((_, s) => s(Question.RELEVANCE)).filter(r => r >= 8).length > 0

@main def tryConferenceReviewingImpl: Unit =
  import ConferenceReviewing.*
  import ConferenceReviewing.Question.*

  val cr: ConferenceReviewing = ConferenceReviewing()

  // init()
  cr.loadReview(1, 8, 8, 6, 8)
  cr.loadReview(1, 9, 9, 6, 9)
  cr.loadReview(2, 9, 9, 10, 9)
  cr.loadReview(2, 4, 6, 10, 6)
  cr.loadReview(3, 3, 3, 3, 3)
  cr.loadReview(3, 4, 4, 4, 4)
  cr.loadReview(4, 6, 6, 6, 6)
  cr.loadReview(4, 7, 7, 8, 7)
  val map: Map[ConferenceReviewing.Question, Int] = Map(
    Question.RELEVANCE -> 8,
    Question.SIGNIFICANCE -> 8,
    Question.CONFIDENCE -> 7,
    Question.FINAL -> 8
  )
  cr.loadReview(4, map)
  cr.loadReview(5, 6, 6, 6, 10)
  cr.loadReview(5, 7, 7, 7, 10)

  // testOrderedScores()
  println("cr.orderedScores(2,Question.RELEVANCE): " + cr.orderedScores(2,Question.RELEVANCE))
  println("cr.orderedScores(4,Question.CONFIDENCE): " + cr.orderedScores(4,Question.CONFIDENCE))
  println("cr.orderedScores(5,Question.FINAL): " + cr.orderedScores(5,Question.FINAL))

  // testAverageFinalScore()
  println("averageFinalScore(1): " + cr.averageFinalScore(1))
  println("averageFinalScore(2): " + cr.averageFinalScore(2))
  println("averageFinalScore(3): " + cr.averageFinalScore(3))
  println("averageFinalScore(4): " + cr.averageFinalScore(4))
  println("averageFinalScore(5): " + cr.averageFinalScore(5))

  // testAccepted() // must be private method
  println("\n--- Test Accepted ---")
  println("cr.accepted(1): " + cr.accepted(1))
  println("cr.accepted(2): " + cr.accepted(2))
  println("cr.accepted(3): " + cr.accepted(3))
  println("cr.accepted(4): " + cr.accepted(4))
  println("cr.accepted(5): " + cr.accepted(5))

  // testAcceptedArticles
  println("cr.acceptedArticles(): " + cr.acceptedArticles())

  // testSortedAcceptedArticles
  println("cr.sortedAcceptedArticles(): " + cr.sortedAcceptedArticles())

  // testSortedAcceptedArticles



