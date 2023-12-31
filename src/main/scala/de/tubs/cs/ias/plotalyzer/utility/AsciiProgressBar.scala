package de.tubs.cs.ias.plotalyzer.utility

import me.tongfei.progressbar.{
  ProgressBar,
  ProgressBarBuilder,
  ProgressBarStyle
}

object AsciiProgressBar {

  def create(name: String, length: Long): ProgressBar = {
    new ProgressBarBuilder()
      .setStyle(ProgressBarStyle.ASCII)
      .setTaskName(name)
      .setInitialMax(length)
      .build()
  }

}
