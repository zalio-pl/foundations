package exercises.action.fp.search

// `flights` must be ordered using `SearchResult.bestOrdering`
case class SearchResult private (flights: List[Flight]) {
  val cheapest: Option[Flight] = flights.minByOption(_.unitPrice)
  val fastest: Option[Flight]  = flights.minByOption(_.duration)
  val best: Option[Flight]     = flights.minOption(SearchResult.bestOrdering)
}

object SearchResult {
  def apply(flights: List[Flight]): SearchResult = {
    val flightsWithoutDuplicates = flights
      .groupBy(_.flightId)
      .map { case (_, flightsWithSameId) => flightsWithSameId.minBy(_.unitPrice) }
      .toList
    val sortedFlights = flightsWithoutDuplicates.sorted(bestOrdering)
    new SearchResult(sortedFlights)
  }
  // Order by number of stops (0, 1, 2, ...) and then by price.
  // For example, sorting the following flights
  // flight A: 2 stops, 100$
  // flight B: 0 stop , 120$
  // flight C: 1 stop ,  50$
  // flight D: 0 stop ,  80$
  // produces: flight D, flight B, flight C, flight A
  val bestOrdering: Ordering[Flight] =
    Ordering.by(flight => (flight.numberOfStops, flight.unitPrice))

}
