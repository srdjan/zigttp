// Weather Forecast - a small web app on the zttp runtime.
//
// GET /          serves the frontend UI: latitude/longitude inputs and a
//                "Get Weather" button. The page fetches /forecast client-side.
// GET /forecast  reads the user's latitude & longitude from the query string
//                and fetches the current conditions from the keyless
//                Open-Meteo API, then returns the parsed JSON.
//
// The fetch URL is a compile-time literal, so the contract proves the only
// egress host is api.open-meteo.com. The user-supplied coordinates ride in the
// dynamic `query` init object (percent-encoded at the boundary), so they can
// vary per request without making the egress host unprovable.
//
// Run:
//   zttp serve examples/fetch/weather-app.ts \
//     --outbound-host api.open-meteo.com -p 3000
//
// Try it:
//   open http://localhost:3000
//   curl "http://localhost:3000/forecast?latitude=40.71&longitude=-74.01"

import type { Spec } from "zttp:types";
import { fetch } from "zttp:fetch";

type WeatherProof = Spec<"state_isolated" | "no_secret_leakage">;

// The frontend UI. A static page: the form posts nothing - its submit handler
// fetches /forecast and renders the result. (Browser-side script only; the
// zttp engine just serves this string.)
function page(): string {
  return `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Weather Forecast</title>
<style>
  :root { color-scheme: light dark; }
  body { font-family: system-ui, -apple-system, sans-serif; max-width: 34rem; margin: 4rem auto; padding: 0 1rem; line-height: 1.5; }
  h1 { font-size: 1.6rem; margin-bottom: .25rem; }
  .muted { opacity: .65; font-size: .9rem; }
  form { display: flex; gap: .75rem; flex-wrap: wrap; align-items: flex-end; margin-top: 1.5rem; }
  label { display: flex; flex-direction: column; gap: .3rem; font-size: .8rem; text-transform: uppercase; letter-spacing: .04em; }
  input { padding: .55rem .6rem; font-size: 1rem; width: 8rem; border: 1px solid currentColor; border-radius: .5rem; background: transparent; color: inherit; }
  button { padding: .6rem 1.2rem; font-size: 1rem; border: 0; border-radius: .5rem; cursor: pointer; background: #2563eb; color: #fff; }
  button:hover { background: #1d4ed8; }
  #out { margin-top: 1.75rem; }
  .card { padding: 1.25rem 1.5rem; border-radius: .9rem; border: 1px solid color-mix(in srgb, currentColor 18%, transparent); }
  .temp { font-size: 2.75rem; font-weight: 650; line-height: 1; }
  .row { margin-top: .4rem; }
</style>
</head>
<body>
<h1>Weather Forecast</h1>
<p class="muted">Powered by zttp - native fetch, zero dependencies, proven egress.</p>
<form id="f">
  <label>Latitude <input id="lat" type="number" step="any" value="52.52" required></label>
  <label>Longitude <input id="lng" type="number" step="any" value="13.41" required></label>
  <button type="submit">Get Weather</button>
</form>
<div id="out"></div>
<script>
  var f = document.getElementById("f");
  var out = document.getElementById("out");
  f.addEventListener("submit", function (e) {
    e.preventDefault();
    var lat = document.getElementById("lat").value;
    var lng = document.getElementById("lng").value;
    out.textContent = "Loading...";
    var url = "/forecast?latitude=" + encodeURIComponent(lat) + "&longitude=" + encodeURIComponent(lng);
    fetch(url).then(function (r) { return r.json(); }).then(function (d) {
      if (d.error) { out.textContent = "Error: " + d.error; return; }
      var c = d.current;
      // Build the result with textContent (no innerHTML) so values are never
      // interpreted as markup.
      var mk = function (cls, text) { var el = document.createElement("div"); el.className = cls; el.textContent = text; return el; };
      var card = document.createElement("div");
      card.className = "card";
      card.appendChild(mk("temp", c.temperature + c.temperatureUnit));
      card.appendChild(mk("row", "Humidity " + c.humidity + c.humidityUnit + " · Wind " + c.windSpeed + c.windSpeedUnit));
      card.appendChild(mk("row muted", d.timezone + " · lat " + d.coordinates.latitude + ", lon " + d.coordinates.longitude));
      out.replaceChildren(card);
    }).catch(function () { out.textContent = "Request failed."; });
  });
</script>
</body>
</html>`;
}

// GET /          serves the frontend UI.
// GET /forecast  reads the user's coordinates from the query string, fetches
//                the current conditions for them, and returns the parsed JSON.
function handler(req: Request): Response & WeatherProof {
  if (req.method !== "GET") {
    return Response.json({ error: "method_not_allowed" }, { status: 405 });
  }

  if (req.path === "/") {
    return Response.html(page());
  }

  if (req.path !== "/forecast") {
    return Response.json({ error: "not_found" }, { status: 404 });
  }

  const lat = req.query.latitude;
  const lng = req.query.longitude;
  if (lat === undefined || lng === undefined) {
    return Response.json({
      error: "missing_coordinates",
      hint: "provide latitude and longitude query params",
    }, { status: 400 });
  }

  // The fetch URL is a compile-time literal, so the contract proves the egress
  // host is api.open-meteo.com. The user's coordinates ride in the dynamic
  // `query` object and are percent-encoded at the boundary.
  const upstream = fetch("https://api.open-meteo.com/v1/forecast", {
    query: {
      latitude: lat,
      longitude: lng,
      current: "temperature_2m,relative_humidity_2m,wind_speed_10m,is_day",
      timezone: "auto",
    },
    headers: { "Accept": "application/json" },
    maxResponseBytes: 65536,
  });

  if (!upstream.ok) {
    return Response.json({
      error: "weather_unavailable",
      upstreamStatus: upstream.status,
    }, { status: 502 });
  }

  const f = upstream.json();
  return Response.json({
    app: "Weather Forecast",
    source: "open-meteo",
    coordinates: {
      latitude: f.latitude,
      longitude: f.longitude,
    },
    timezone: f.timezone,
    current: {
      time: f.current.time,
      temperature: f.current.temperature_2m,
      temperatureUnit: f.current_units.temperature_2m,
      humidity: f.current.relative_humidity_2m,
      humidityUnit: f.current_units.relative_humidity_2m,
      windSpeed: f.current.wind_speed_10m,
      windSpeedUnit: f.current_units.wind_speed_10m,
      isDay: f.current.is_day,
    },
  });
}
