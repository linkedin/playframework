/*
 * Copyright (C) 2009-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package play.api.mvc

import org.specs2.mutable._
import play.core.test._

object CookiesSpec extends Specification {

  "object Cookies#fromCookieHeader" should {

    "create new Cookies instance with cookies" in withApplication {
      val originalCookie = Cookie(name = "cookie", value = "value")

      val headerString = Cookies.encodeCookieHeader(Seq(originalCookie))
      val c = Cookies.fromCookieHeader(Some(headerString))

      c must beAnInstanceOf[Cookies]
    }

    "should create an empty Cookies instance with no header" in withApplication {
      val c = Cookies.fromCookieHeader(None)
      c must beAnInstanceOf[Cookies]
    }
  }

  "trait Cookies#get" should {
    val originalCookie = Cookie(name = "cookie", value = "value")
    def headerString = Cookies.encodeCookieHeader(Seq(originalCookie))
    def c: Cookies = Cookies.fromCookieHeader(Some(headerString))

    "get a cookie" in withApplication {
      c.get("cookie") must beSome[Cookie].which { cookie =>
        cookie.name must be_==("cookie")
      }
    }

    "return none if no cookie" in withApplication {
      c.get("no-cookie") must beNone
    }
  }

  "trait Cookies#apply" should {
    val originalCookie = Cookie(name = "cookie", value = "value")
    def headerString = Cookies.encodeCookieHeader(Seq(originalCookie))
    def c: Cookies = Cookies.fromCookieHeader(Some(headerString))

    "apply for a cookie" in withApplication {
      val cookie = c("cookie")
      cookie.name must be_==("cookie")
    }

    "throw error if no cookie" in withApplication {
      {
        c("no-cookie")
      }.must(throwA[RuntimeException](message = "Cookie doesn't exist"))
    }
  }

  "trait Cookies#traversable" should {
    val cookie1 = Cookie(name = "cookie1", value = "value2")
    val cookie2 = Cookie(name = "cookie2", value = "value2")

    "be empty for no cookies" in withApplication {
      val c = Cookies.fromCookieHeader(header = None)
      c must be empty
    }

    "contain elements for some cookies" in withApplication {
      val headerString = Cookies.encodeCookieHeader(Seq(cookie1, cookie2))
      val c: Cookies = Cookies.fromCookieHeader(Some(headerString))
      c must contain(allOf(cookie1, cookie2))
    }

    // technically the same as above
    "run a foreach for a cookie" in withApplication {
      val headerString = Cookies.encodeCookieHeader(Seq(cookie1))
      val c: Cookies = Cookies.fromCookieHeader(Some(headerString))

      var myCookie: Cookie = null
      c.foreach { cookie =>
        myCookie = cookie
      }
      myCookie must beEqualTo(cookie1)
    }

    "handle SameSite cookies properly" in {
      val decoded = Cookies.decodeSetCookieHeader("cookie=value; Secure; SameSite=Strict")
      decoded must contain(Cookie("cookie", "value", secure = true, httpOnly = false, sameSite = Some(Cookie.SameSite.Strict)))
    }

    "handle SameSite=None cookie properly" in {
      val decoded = Cookies.decodeSetCookieHeader("cookie=value; Secure; SameSite=None")
      decoded must contain(
        Cookie("cookie", "value", secure = true, httpOnly = false, sameSite = Some(Cookie.SameSite.None))
      )
    }

    "handle SameSite=Lax cookie properly" in {
      val decoded = Cookies.decodeSetCookieHeader("cookie=value; Secure; SameSite=Lax")
      decoded must contain(
        Cookie("cookie", "value", secure = true, httpOnly = false, sameSite = Some(Cookie.SameSite.Lax))
      )
    }
  }

  "merging cookies" should {
    "replace old cookies with new cookies of the same name" in {
      val originalRequest = FakeRequest().withCookies(Cookie("foo", "fooValue1"), Cookie("bar", "barValue2"))
      val requestWithMoreCookies = originalRequest.withCookies(Cookie("foo", "fooValue2"), Cookie("baz", "bazValue"))
      val cookies = requestWithMoreCookies.cookies
      cookies.toSet must_== Set(
        Cookie("foo", "fooValue2"),
        Cookie("bar", "barValue2"),
        Cookie("baz", "bazValue")
      )
    }
    "return one cookie for each name" in {
      val cookies = FakeRequest().withCookies(
        Cookie("foo", "foo1"), Cookie("foo", "foo2"), Cookie("bar", "bar"), Cookie("baz", "baz")
      ).cookies
      cookies.toSet must_== Set(
        Cookie("foo", "foo2"),
        Cookie("bar", "bar"),
        Cookie("baz", "baz")
      )
    }
  }

  "object Cookie.SameSite#parse" should {
    "successfully parse SameSite.None value" in {
      Cookie.SameSite.parse("None") must beSome[Cookie.SameSite](Cookie.SameSite.None)
    }

    "successfully parse SameSite.Lax value" in {
      Cookie.SameSite.parse("Lax") must beSome[Cookie.SameSite](Cookie.SameSite.Lax)
    }

    "successfully parse SameSite.Strict value" in {
      Cookie.SameSite.parse("Strict") must beSome[Cookie.SameSite](Cookie.SameSite.Strict)
    }

    "return Option.None for unknown SameSite value" in {
      Cookie.SameSite.parse("Unknown") must beNone
    }
  }
}
