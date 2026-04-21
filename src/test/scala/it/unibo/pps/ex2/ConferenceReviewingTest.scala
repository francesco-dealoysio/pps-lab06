package it.unibo.pps.ex2

import org.junit.*
import org.junit.Assert.*

class ConferenceReviewingTest:
  import ConferenceReviewing.*
  import ConferenceReviewing.Question.*

  @Before
  val cr: ConferenceReviewing = ConferenceReviewing()
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

  @Test
  def testAverageFinalScore: Unit =
    assertEquals(cr.averageFinalScore(1), 8.5, 0.01)
    assertEquals(cr.averageFinalScore(2), 7.5, 0.01)
    assertEquals(cr.averageFinalScore(3), 3.5, 0.01)
    assertEquals(cr.averageFinalScore(4), 7.0, 0.01)
    assertEquals(cr.averageFinalScore(5), 10.0, 0.01)

  @Test
  def testAccepted: Unit =
    assertTrue(cr.accepted(1))
    assertTrue(cr.accepted(2))
    assertFalse(cr.accepted(3))
    assertTrue(cr.accepted(4))
    assertFalse(cr.accepted(5))
