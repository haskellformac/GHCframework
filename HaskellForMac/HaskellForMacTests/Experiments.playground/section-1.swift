// Playground - noun: a place where people can play

import Cocoa

let myText = "This is a long piece of text, which is spread\n\n\nover multiple lines,\nand we need to figure out how many lines\nthere are."

myText.rangeOfString("which")
let range: Range<String.Index> = advance(myText.startIndex, 47)...advance(myText.startIndex, 49)

myText
myText.lineRangeForRange(range)
myText.lineRangeForRange(myText.startIndex...myText.startIndex)
myText.startIndex

extension String {
  func lineNumberAtLocation(loc: String.Index) -> UInt {
    switch self.lineRangeForRange(loc...loc).startIndex {
    case self.startIndex: return 1
    case let start: return lineNumberAtLocation(advance(start, -1)) + 1
    }
  }
}

myText.lineNumberAtLocation(advance(myText.startIndex, 66))
myText[myText.startIndex...advance(myText.startIndex, 66)]

let myNSText = myText as NSString
let r = myNSText.rangeOfString("which")

myNSText.lineRangeForRange(NSRange(location: 0, length: 0))
myNSText.characterAtIndex(0)

("a\n\n" as NSString).lineRangeForRange(NSRange(location: 2, length: 1))

extension NSString {
  func lineNumberAtLocation(loc: Int) -> Int {
    switch self.lineRangeForRange(NSRange(location: loc, length: 0)).location {
    case 0: return 1
    case let start: return lineNumberAtLocation(start - 1) + 1
    }
  }
}

myNSText.lineNumberAtLocation(67)
