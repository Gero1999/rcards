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

        def action_control(control, label):
            expect(control).to_be_visible()
            label_pattern = re.compile(re.escape(label), re.IGNORECASE)
            text = control.locator(".rc-action-label")
            expect(text).to_be_visible()
            expect(text).to_have_text(label_pattern)
            expect(control).to_have_accessible_name(label_pattern)
            expect(control).to_have_attribute("title", re.compile(r"\S"))
            icon = control.locator(".rc-action-icon")
            expect(icon).to_be_visible()
            expect(icon).to_have_attribute("aria-hidden", "true")
            return control

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
        own = page.locator(".rc-own")
        board_controls = page.locator(".rc-board-controls")
        hand_controls = own.locator(".rc-hand-controls")
        action_control(board_controls.locator('[data-rc-action="call"]'), "call 1")
        fold = action_control(hand_controls.locator('[data-rc-action="fold"]'), "fold")
        odds = action_control(hand_controls.locator("#show_holdem_odds"), "Hand odds")
        raise_toggle = action_control(board_controls.locator("[data-rc-raise-toggle]"), "Raise")
        expect(page.locator(".rc-seat .rc-back")).to_have_count(2)
        stock = page.locator('.rc-stock[data-zone="stock"]')
        expect(stock).to_be_visible()
        expect(stock.locator(".rc-rank, .rc-suit")).to_have_count(0)
        amount = board_controls.locator("#amount")
        confirm_raise = board_controls.locator('[data-rc-action="raise"]')
        expect(amount).not_to_be_visible()
        expect(confirm_raise).not_to_be_visible()
        before = version()
        odds.click()
        comparison = page.get_by_role("dialog", name="Hand odds")
        expect(comparison.get_by_text("Win at showdown", exact=True)).to_be_visible()
        comparison.get_by_role("button", name="Back to the table").click()
        expect(comparison).not_to_be_visible()
        expect(page.locator(".modal-backdrop")).to_have_count(0)
        expect(own.locator(".rc-card").first).to_have_attribute("data-rc-version", before)
        screenshot(page, "table-holdem.png")

        raise_toggle.click()
        expect(amount).to_be_focused()
        expect(amount).to_have_attribute("min", "4")
        expect(amount).to_have_attribute("max", "100")
        amount.fill("3")
        expect(amount).to_have_attribute("aria-invalid", "true")
        expect(confirm_raise).to_be_disabled()
        amount.press("Escape")
        expect(amount).not_to_be_visible()
        expect(raise_toggle).to_be_focused()
        expect(own.locator(".rc-card").first).to_have_attribute("data-rc-version", before)

        raise_toggle.click()
        expect(amount).to_have_value("4")
        amount.fill("8")
        board_controls.locator("[data-rc-raise-cancel]").click()
        expect(amount).not_to_be_visible()
        expect(raise_toggle).to_be_focused()
        expect(own.locator(".rc-card").first).to_have_attribute("data-rc-version", before)

        viewport = page.viewport_size
        page.set_viewport_size({"width": 390, "height": 844})
        raise_toggle.click()
        expect(amount).to_have_value("4")
        amount.fill("8")
        expect(confirm_raise).to_have_accessible_name("Raise to 8")
        if not board_controls.evaluate("(controls) => controls.scrollWidth <= controls.clientWidth"):
            raise AssertionError("Hold'em Raise editor has horizontal overflow on mobile")
        expect(confirm_raise).to_have_attribute("data-rc-version", before)
        expect(own.locator(".rc-card").first).to_have_attribute("data-rc-version", before)
        confirm_raise.click()
        expect_action_applied(before)
        expect(page.locator(".rc-history")).to_contain_text("You / raise 8")
        expect(amount).not_to_be_visible()
        page.set_viewport_size(viewport)

        # Reset the session so the independent Fold path starts from the same blinds.
        page.reload(wait_until="networkidle")
        before = version()
        raise_toggle.click()
        expect(amount).to_be_visible()
        # A fold finishes the hand, then Next hand continues the same table.
        fold.click()
        expect_action_applied(before)
        expect(amount).not_to_be_visible()
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
        viewport = page.viewport_size
        page.set_viewport_size({"width": 390, "height": 844})
        page.locator("#show_scopa_odds").click()
        comparison = page.get_by_role("dialog", name="Compare moves")
        expect(comparison.locator("tbody tr").first).to_be_visible()
        if not comparison.locator(".table-responsive").evaluate(
                "(table) => table.scrollWidth <= table.clientWidth"):
            raise AssertionError("Mobile Scopa comparison has horizontal overflow")
        comparison.locator(".modal-footer button").click()
        expect(comparison).not_to_be_visible()
        page.set_viewport_size(viewport)
        expect(page.locator('[data-rc-card="1"][data-rc-zone="hand"]')).to_have_attribute(
            "aria-pressed", "true")
        expect(page.locator('input[name="capture"]').first).to_be_checked()
        expect(page.locator(".rc-own .rc-card").first).to_have_attribute("data-rc-version", before)
        page.locator('[data-rc-action="play"]').click()
        expect_action_applied(before)
        expect(page.locator('.rc-own [data-zone="hand"] .rc-card')).to_have_count(2)

        new_table("blackjack", 1, 1, "Blackjack")
        stand = hand_controls.locator('[data-rc-action="stand"]')
        # Look for a playable opening round even if a natural settles one.
        for attempt in range(4):
            stand.or_(page.locator("#next_hand")).first.wait_for(state="visible")
            if stand.count():
                break
            expect(page.locator('[data-zone="dealer"] .rc-back')).to_have_count(0)
            expect(page.locator(".rc-result").first).to_be_visible()
            if attempt < 3:
                before = version()
                page.locator("#new_table").click()
                expect_action_applied(before)
        viewport = page.viewport_size
        page.set_viewport_size({"width": 390, "height": 844})
        shoe = page.locator('.rc-board-cards .rc-stock[data-zone="shoe"]')
        expect(shoe).to_be_visible()
        expect(shoe.locator(".rc-rank, .rc-suit")).to_have_count(0)
        action_control(board_controls.locator('[data-rc-action="hit"]'), "hit")
        double = board_controls.locator('[data-rc-action="double"]')
        if page.locator('[data-rc-action="double"]').count():
            action_control(double, "double")
        action_control(stand, "stand")
        split = hand_controls.locator('[data-rc-action="split"]')
        if page.locator('[data-rc-action="split"]').count():
            action_control(split, "split")
        odds = action_control(hand_controls.locator("#show_blackjack_odds"), "Compare actions")
        if not own.evaluate("(hand) => hand.scrollWidth <= hand.clientWidth"):
            raise AssertionError("Blackjack hand controls have horizontal overflow on mobile")
        before = version()
        expect(page.locator('[data-zone="dealer"] .rc-back')).to_have_count(1)
        odds.click()
        comparison = page.get_by_role("dialog", name="Compare actions")
        expect(comparison.locator("tbody").get_by_role("rowheader", name="stand")).to_be_visible()
        comparison.get_by_role("button", name="Back to the table").click()
        expect(comparison).not_to_be_visible()
        expect(own.locator(".rc-card").first).to_have_attribute("data-rc-version", before)
        stand.click()
        expect_action_applied(before)
        page.set_viewport_size(viewport)
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

        new_table("liar", 2, 7, "Mentiroso")
        claim = action_control(own.locator('[data-rc-action="claim"]'), "claim")
        rank = own.locator("#rank")
        card = own.locator('[data-rc-card="1"][data-rc-zone="hand"]')
        # A truthful claim ensures the other player leads the next claim.
        card_count = own.locator(".rc-card").count()
        rank_value = card.locator(".rc-rank").first.inner_text().strip()
        rank.select_option(rank_value)
        card.press("Space")
        expect(card).to_have_attribute("aria-pressed", "true")
        expect(card).to_be_focused()
        expect(rank).to_have_value(rank_value)
        before = version()
        claim.click()
        expect_action_applied(before)
        expect(page.locator(".rc-history")).to_contain_text(
            f"Claim: You | Rank: {rank_value} | Cards: 1")
        expect(own.locator(".rc-card")).to_have_count(card_count - 1)
        center = page.locator(".rc-center")
        challenge = action_control(center.locator('[data-rc-action="challenge"]'), "challenge")
        action_control(center.locator('[data-rc-action="pass"]'), "pass")
        expect(rank).to_have_count(0)
        expect(own.locator("[data-rc-card]")).to_have_count(0)
        before = version()
        challenge.click()
        expect_action_applied(before)
        expect(page.locator(".rc-history")).to_contain_text("You / challenge")
        expect(center.locator('[data-zone="reveal"] .rc-card').first).to_be_visible()
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
        print("Passed: five games, card-adjacent actions, three analysis dialogs, Mentiroso responses, continued poker/Blackjack, logo, contextual Help, languages and mobile layout.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--url", type=local_url, default="http://127.0.0.1:8765/",
                        help="Fresh default rcards app; loopback addresses only")
    parser.add_argument("--chromium", help="Optional Chromium executable path")
    parser.add_argument("--screenshots", help="Optional output directory for screenshots")
    run(parser.parse_args())
