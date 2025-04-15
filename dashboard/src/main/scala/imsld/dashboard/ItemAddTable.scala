package imsld.dashboard

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.HTMLDivElement
import cats.syntax.all.*
import java.time.LocalDate
import scala.util.Try
import java.text.NumberFormat
import java.util.Locale

object ItemAddTable:
  private val itemsVar: Var[List[ItemDtoFlat]] = Var(List(ItemDtoFlat()))

  def apply(): ReactiveHtmlElement[HTMLDivElement] =
    div(
      controlPanel,
      children <-- itemsVar.signal.splitByIndex { (idx, _, signal) =>
        div(text <-- signal.map(_.toString()))
      },
      inputForm
    )

  private val controlPanel: ReactiveHtmlElement[HTMLDivElement] =
    div(
      button(
        typ := "button",
        onClick --> itemsVar.updater { (prev, _) =>
          prev.appended(ItemDtoFlat())
        },
        "Add"
      )
    )

  private val inputForm =
    form(
      table(
        thead(
          tr(
            List(
              "slug",
              "label",
              "acquire date",
              "acquire price",
              "acquire source",
              ""
            ).map { h => th(h) }
          )
        ),
        tbody(
          children <-- itemsVar.signal.splitByIndex { (idx, _, signal) =>
            TableBodyRow(idx, signal)
          }
        )
      )
    )

  private def TableBodyRow(idx: Int, signal: Signal[ItemDtoFlat]) =
    val slugCell = td(
      input(
        controlled(
          value <-- signal.map(_.slug.getOrElse("")),
          onInput.mapToValue --> itemsVar.updater[String] { (lst, str) =>
            lst.updated(
              idx,
              lst(idx)
                .copy(slug = if (str.isEmpty()) None else str.some)
            )
          }
        )
      )
    )

    val labelCell = td(
      textArea(
        controlled(
          value <-- signal.map(_.label.getOrElse("")),
          onInput.mapToValue --> itemsVar.updater[String] { (lst, str) =>
            val processedStr = str.filter { c =>
              !"\r\n".contains(c)
            }
            lst.updated(
              idx,
              lst(idx)
                .copy(label =
                  if (processedStr.isEmpty()) None
                  else processedStr.some
                )
            )
          }
        )
      )
    )

    val acquireDateCell = td(
      input(
        typ := "date",
        controlled(
          value <-- signal.map(
            _.acquireDate.fold("")(_.toString())
          ),
          onInput.mapToValue --> itemsVar
            .updater[String] { (lst, str) =>
              lst.updated(
                idx,
                lst(idx)
                  .copy(acquireDate = Try(LocalDate.parse(str)).toOption)
              )
            }
        )
      )
    )

    def acquirePriceCell =
      val lastGoodPriceValue: Var[String] = Var("")
      td(
        input(
          cls := "ItemAddTable-acquirePriceCell-ccy",
          placeholder := "ccy",
          controlled(
            value <-- signal.map(_.acquirePriceCurrency.getOrElse("")),
            onInput.mapToValue --> itemsVar
              .updater[String] { (lst, str) =>
                lst.updated(
                  idx,
                  lst(idx).copy(acquirePriceCurrency =
                    if (str.isEmpty()) None else str.some
                  )
                )
              }
          ),
          listId := "defaultCurrencies",
          size := 8
        ),
        dataList(
          idAttr := "defaultCurrencies",
          List("SGD", "USD", "EUR", "VND").map { ccy =>
            option(value := ccy)
          }
        ),
        input(
          placeholder := "value",
          controlled(
            value <-- lastGoodPriceValue.signal,
            onInput.mapToValue --> lastGoodPriceValue.writer
          ),
          lastGoodPriceValue.signal --> itemsVar.updater[String] { (lst, str) =>
            lst.updated(
              idx,
              lst(idx).copy(acquirePriceValue =
                Try(BigDecimal(str.filter(_ != ','))).toOption
              )
            )
          },
          signal.map(_.acquirePriceValue) --> lastGoodPriceValue
            .updater[Option[BigDecimal]] { (prev, opt) =>
              opt.fold(prev) { bd =>
                val formatter = NumberFormat.getInstance(Locale.US)
                formatter.setMaximumFractionDigits(bd.scale)
                formatter.setMinimumFractionDigits(bd.scale)
                formatter.format(bd)
              }
            },
          size := 16
        )
      )

    val acquireSourceCell = td(
      textArea(
        controlled(
          value <-- signal.map(_.acquireSource.getOrElse("")),
          onInput.mapToValue --> itemsVar
            .updater[String] { (lst, str) =>
              lst.updated(
                idx,
                lst(idx).copy(acquireSource =
                  if (str.isEmpty()) None else str.some
                )
              )
            }
        )
      )
    )

    val removeRowCell = td(
      button(
        typ := "button",
        onClick --> itemsVar.updater { (lst, _) =>
          lst.zipWithIndex.collect { case (item, i) if i != idx => item }
        },
        "Remove"
      )
    )

    tr(
      slugCell,
      labelCell,
      acquireDateCell,
      acquirePriceCell,
      acquireSourceCell,
      removeRowCell
    )

  private final case class ItemDtoFlat(
      slug: Option[String] = None,
      label: Option[String] = None,
      acquireDate: Option[LocalDate] = None,
      acquirePriceCurrency: Option[String] = None,
      acquirePriceValue: Option[BigDecimal] = None,
      acquireSource: Option[String] = None
  )
