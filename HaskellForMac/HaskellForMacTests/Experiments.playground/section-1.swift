// Playground - noun: a place where people can play

import Cocoa

let textView = NSTextView()
var paragraphStyle = NSParagraphStyle.defaultParagraphStyle().mutableCopy() as NSMutableParagraphStyle
paragraphStyle.lineBreakMode = .ByTruncatingTail
textView.defaultParagraphStyle = paragraphStyle

let menlo13 = NSFont(name: "Menlo-Regular", size:13)

let attributes = [NSFontAttributeName: menlo13, NSParagraphStyleAttributeName: paragraphStyle]

