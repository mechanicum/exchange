package ru

package object mek {
    object PaperName extends Enumeration {
        val A, B, C, D = Value
    }

    type PaperName = PaperName.Value
}
