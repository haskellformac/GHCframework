//
//  PlaygroundResultStorage.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 19/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  This class is the view model for the results displayed in a playground and serves as the data source of the table
//  view displaying the results.

import Cocoa

// Table view cell identifiers
//
let kTypeCell  = "TypeCell"
let kValueCell = "ValueCell"

/// Represents the result for a single command.
///
struct Result {
  // FIXME: for now, it's all strings, but we want to be more flexible in the future.

  let value:  String
  let type:   String
  let height: CGFloat       // cell height — FIXME: this should be computed from the text view's layout manager instead of being chached
  let stale:  Bool          // a result is stale while it is being recomputed
}

class PlaygroundResultStorage: NSObject {

  // The results for all commands in the playground if successfully computed.
  //
  // NB: The length of the array always equals the number of commands in the playground.
  //
  private var results: [Result?] = []

  /// Type of callback to advise results view that all rows need to be redisplayed.
  ///
  typealias Redisplay = () -> ()

  /// Type of callback to advise results view that the row for the given command index needs to be redisplayed.
  ///
  typealias RedisplayRow = Int -> ()

  private let redisplay:    Redisplay
  private let redisplayRow: RedisplayRow

  init(redisplay: Redisplay, redisplayRow: RedisplayRow) {
    self.redisplay    = redisplay
    self.redisplayRow = redisplayRow
  }

  /// Reports a result at a specific index. Allocates new results slots if needed.
  ///
  func reportResult(result: String, type: String, height: CGFloat, atCommandIndex idx: Int) {

      // Extend the array to include the reported index if necessary.
    if idx >= results.endIndex {
      for i in results.endIndex...idx { results.append(nil) }
    }
    results[idx] = Result(value: result, type: type, height: height, stale: false)
    redisplayRow(idx)
  }

  /// Discard all entries from the given index on.
  ///
  func pruneAt(idx: Int) {
    if idx < results.endIndex {
      results.removeRange(idx..<results.endIndex)
    }
    redisplay()
  }

  /// Marks all current results as stale.
  ///
  func invalidate() {
    results = results.map{ result in
      if let result = result {
        return Result(value: result.value, type: result.type, height: result.height, stale: true)
      } else { return nil }
    }
    redisplay()
  }

  /// Retrieve the result at the given command index.
  ///
  func queryResult(idx: Int) -> Result? {
    if idx < results.endIndex {
      return results[idx]
    } else { return nil }
  }
}


// MARK: -
// MARK: NSTableViewDelegate & NSTableViewDataSource protocol methods (for the result view)

extension PlaygroundResultStorage: NSTableViewDataSource {

  func numberOfRowsInTableView(_tableView: NSTableView) -> Int {
    return results.count
  }
}
