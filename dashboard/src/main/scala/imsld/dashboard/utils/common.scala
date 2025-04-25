package imsld.dashboard.utils

private def sanitiseText(str: String): Option[String] =
  val trimmed = str.trim()
  if (trimmed.isEmpty()) None else Option(trimmed)
