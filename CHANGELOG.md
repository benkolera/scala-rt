Changes to com.benkolera scala-rt
=================================

2015-11-02 - 0.6.0 - Ben Kolera
- Upgraded to scala 2.11 and scalaz 7.1
- Added gte and lte to the query builder. 

2013-10-11 - 0.5.0 - Ben Kolera
- Changed QueryBuilder to use NonEmptyLists to avoid bad queries being generated (e.g TicketId.in( Seq.empty:_* ) used to generate a bad "()" query string. Now it isn't possible to compile that.
- This fixed a bug in the pagination where an empty search crashed RT with a bad query.

2013-10-04 - 0.4.5 - Ben Kolera
-------------------------------
- Bumped to scala 2.10.3
- Published to nexus.benkolera.com