#!/usr/bin/env python3
"""Smoke-test a running, fresh rcards::play_app() with local browser traffic.

Requires Python Playwright and Chromium. Start the R app separately; this
script never installs dependencies or launches a server. Optional screenshots
are written only when --screenshots is supplied.
"""

import argparse
import ipaddress
import json
import re
from pathlib import Path
from urllib.parse import urlsplit

from playwright.sync_api import expect, sync_playwright


def origin(url):
    parsed = urlsplit(url)
    default_port = 443 if parsed.scheme in ("https", "wss") else 80
    return parsed.scheme, parsed.hostname, parsed.port or default_port


def local_url(value):
    parsed = urlsplit(value)
    if parsed.scheme not in ("http", "https") or parsed.username or parsed.password:
        raise argparse.ArgumentTypeError("Use an http(s) loopback URL without credentials")
    try:
        loopback = ipaddress.ip_address(parsed.hostname or "").is_loopback
    except ValueError:
        loopback = parsed.hostname == "localhost"
    if not loopback:
        raise argparse.ArgumentTypeError("Only loopback addresses or localhost are allowed")
    try:
        parsed.port
    except ValueError as error:
        raise argparse.ArgumentTypeError(str(error)) from error
    return value


def run(args):
    expected_origin = origin(args.url)
    errors, console_errors, failed, external = [], [], [], []
    screenshot_dir = Path(args.screenshots) if args.screenshots else None
    if screenshot_dir:
        screenshot_dir.mkdir(parents=True, exist_ok=True)

    def screenshot(page, name):
        if screenshot_dir:
            page.screenshot(path=str(screenshot_dir / name), full_page=True)

    with sync_playwright() as playwright:
        launch = {"headless": True}
        if args.chromium:
            launch["executable_path"] = args.chromium
        browser = playwright.chromium.launch(**launch)
        context = browser.new_context(viewport={"width": 1280, "height": 1000},
                                      service_workers="block")

        def route_request(route):
            url = route.request.url
            if url.startswith("data:") or origin(url) == expected_origin:
                route.continue_()
            else:
                external.append(url)
                route.abort()

        def route_websocket(route):
            scheme, host, port = origin(route.url)
            web_origin = ("https" if scheme == "wss" else "http", host, port)
            if web_origin == expected_origin:
                route.connect_to_server()
            else:
                external.append(route.url)
                route.close(code=1008, reason="Only the local app is allowed")

        context.route("**/*", route_request)
        context.route_web_socket("**/*", route_websocket)
        page = context.new_page()
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.on("console", lambda message: console_errors.append(message.text)
                if message.type == "error" else None)
        page.on("response", lambda response: failed.append([response.status, response.url])
                if response.status >= 400 else None)
        page.set_default_timeout(15000)
        page.goto(args.url, wait_until="networkidle")

        def version():
            return page.locator(".rc-own .rc-card").first.get_attribute("data-rc-version")

        def expect_action_applied(previous):
            expect(page.locator(".rc-own .rc-card").first).not_to_have_attribute(
                "data-rc-version", previous)
            expect(page.locator(".rc-error")).to_have_count(0)

        def new_table(kind, player_count, choice_count, title):
            page.select_option("#game_kind", kind)
            expect(page.locator("#players option")).to_have_count(choice_count)
            page.select_option("#players", str(player_count))
            page.locator("#new_table").click()
            expect(page.locator(".rc-game-title")).to_contain_text(title)
            expect(page.locator(".rc-error")).to_have_count(0)

        def open_help(kind, title="Help"):
            page.locator("#show_help").click()
            dialog = page.locator("#shiny-modal")
            expect(dialog).to_be_visible()
            expect(dialog).to_have_css("opacity", "1")
            expect(dialog.locator(".modal-dialog")).to_have_css(
                "transform", re.compile(r"^(none|matrix\(1, 0, 0, 1, 0, 0\))$"))
            expect(dialog.locator("#rcards-help-title")).to_have_text(title)
            expect(dialog.locator("[data-rc-help-game]")).to_have_attribute(
                "data-rc-help-game", kind)
            expect(dialog.locator('a[href^="https://github.com/Gero1999/rcards/issues/new"]')).to_have_count(2)
            return dialog

        def close_help():
            page.locator("#shiny-modal .modal-footer button").click()
            expect(page.locator("#shiny-modal")).not_to_be_visible()
            expect(page.locator(".modal-backdrop")).to_have_count(0)

        # The brand embeds the original SVG; loading it must not request a CDN.
        expect(page.locator(".rc-logo")).to_have_attribute("src", re.compile(r"^data:image/svg\+xml"))
        page.wait_for_function("document.querySelector('.rc-logo').complete && document.querySelector('.rc-logo').naturalWidth > 0")
        open_help("holdem")
        close_help()
        # Setup controls describe a future table. Help follows the actual game.
        page.select_option("#game_kind", "scopa")
        expect(page.locator("#players option")).to_have_count(3)
        open_help("holdem")
        close_help()
        page.select_option("#game_kind", "holdem")

        # A fresh default table starts with the human on the small blind.
        expect(page.locator('[data-rc-action="call"]')).to_be_visible()
        expect(page.locator(".rc-seat .rc-back")).to_have_count(2)
        expect(page.locator("#amount")).to_have_attribute("min", "4")
        expect(page.locator("#amount")).to_have_attribute("max", "100")
        screenshot(page, "table-holdem.png")
        # A fold finishes the hand, then Next hand continues the same table.
        page.locator('[data-rc-action="fold"]').click()
        expect(page.locator("#next_hand")).to_have_text("Next hand")
        expect(page.locator(".rc-history")).to_contain_text("You / fold")
        page.locator("#next_hand").click()
        expect(page.locator(".rc-status").first).to_contain_text(
            re.compile(r"Hand 2\s*\|\s*Dealer Bot 1\s*\|\s*Blinds 1 / 2"))
        expect(page.locator(".rc-error")).to_have_count(0)
        screenshot(page, "table-holdem-next-hand.png")

        new_table("scopa", 3, 3, "Scopa")
        expect(page.locator('[data-zone="table"] .rc-index')).to_have_count(4)
        open_help("scopa")
        close_help()
        screenshot(page, "table-scopa.png")
        page.locator('[data-rc-card="1"][data-rc-zone="hand"]').click()
        expect(page.locator('input[name="capture"]').first).to_be_visible()
        page.locator('input[name="capture"]').first.check()
        before = version()
        page.locator('[data-rc-action="play"]').click()
        expect_action_applied(before)
        expect(page.locator('.rc-own [data-zone="hand"] .rc-card')).to_have_count(2)

        new_table("blackjack", 1, 1, "Blackjack")
        # Naturals may settle in the constructor, without a player decision.
        page.locator('[data-rc-action="stand"], #next_hand').first.wait_for(state="visible")
        if page.locator('[data-rc-action="stand"]').count():
            expect(page.locator('[data-zone="dealer"] .rc-back')).to_have_count(1)
            page.locator('[data-rc-action="stand"]').click()
        else:
            expect(page.locator('[data-zone="dealer"] .rc-back')).to_have_count(0)
            expect(page.locator(".rc-result").first).to_be_visible()
        expect(page.locator("#next_hand")).to_have_text("Next round")
        page.locator("#next_wager").fill("5")
        page.locator("#next_hand").click()
        expect(page.locator(".rc-status").first).to_contain_text("Round 2 | Base wager 5")
        expect(page.locator(".rc-error")).to_have_count(0)
        open_help("blackjack")
        close_help()

        new_table("mus", 4, 1, "Mus")
        expect(page.locator('[data-rc-action="cut"]')).to_be_visible()
        expect(page.locator(".rc-seat .rc-back")).to_have_count(12)
        open_help("mus")
        close_help()

        new_table("liar", 4, 7, "Mentiroso")
        expect(page.locator('[data-rc-action="claim"]')).to_be_visible()
        page.select_option("#rank", "1")
        page.locator('[data-rc-card="1"][data-rc-zone="hand"]').click()
        expect(page.locator('[data-rc-card="1"][data-rc-zone="hand"]')).to_have_attribute(
            "aria-pressed", "true")
        before = version()
        page.locator('[data-rc-action="claim"]').click()
        expect_action_applied(before)
        expect(page.locator(".rc-history")).to_contain_text("Claim: You | Rank: 1 | Cards: 1")
        open_help("liar")
        close_help()

        page.select_option("#language", "it")
        expect(page.locator('label[for="language"]')).to_have_text("Lingua")
        expect(page.locator("#show_help")).to_have_text("Aiuto")
        page.set_viewport_size({"width": 390, "height": 844})
        expect(page.locator(".rc-app")).to_be_visible()
        if not page.evaluate("document.documentElement.scrollWidth <= window.innerWidth"):
            raise AssertionError("Mobile viewport has horizontal overflow")
        screenshot(page, "table-mobile.png")
        dialog = open_help("liar", "Aiuto")
        if not page.evaluate("document.documentElement.scrollWidth <= window.innerWidth"):
            raise AssertionError("Mobile Help dialog has horizontal overflow")
        bounds = dialog.locator(".modal-dialog").bounding_box()
        if not bounds or bounds["x"] < 0 or bounds["x"] + bounds["width"] > 390:
            raise AssertionError("Mobile Help dialog extends beyond the viewport")
        screenshot(page, "table-mobile-help.png")
        close_help()

        report = {"page_errors": errors, "console_errors": console_errors,
                  "failed_requests": failed, "blocked_external_requests": external}
        print(json.dumps(report, indent=2), flush=True)
        browser.close()
        if any(report.values()):
            raise AssertionError("Browser reported errors or attempted non-local traffic")
        print("Passed: five games, continued poker/Blackjack, logo, contextual Help, languages and mobile layout.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--url", type=local_url, default="http://127.0.0.1:8765/",
                        help="Fresh default rcards app; loopback addresses only")
    parser.add_argument("--chromium", help="Optional Chromium executable path")
    parser.add_argument("--screenshots", help="Optional output directory for screenshots")
    run(parser.parse_args())
