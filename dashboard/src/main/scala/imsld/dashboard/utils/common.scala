package imsld.dashboard.utils

import java.text.NumberFormat
import java.util.Locale

def sanitiseText(str: String): Option[String] =
  val trimmed = str.trim()
  if (trimmed.isEmpty()) None else Option(trimmed)

object BigDecimalFormatter:
  private val formatter = NumberFormat.getInstance(Locale.US)
  def format(bd: BigDecimal): String =
    formatter.setMaximumFractionDigits(bd.scale)
    formatter.setMinimumFractionDigits(bd.scale)
    formatter.format(bd)
