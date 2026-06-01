// Weather Forecasts demonstrates a standard outbound fetch and JSON parsing
// against the keyless Open-Meteo API.
//
// Run:
//   zigttp serve examples/fetch/weather-forecasts.ts \
//     --outbound-host api.open-meteo.com -p 3000
//
// Try it:
//   curl http://localhost:3000/weather

import type { Spec } from "zigttp:types";
import { fetch } from "zigttp:fetch";

type WeatherProof = Spec<"state_isolated" | "no_secret_leakage">;

function handler(req: Request): Response & WeatherProof {
  if (req.method !== "GET") {
    return Response.json({ error: "method_not_allowed" }, { status: 405 });
  }

  if (req.path !== "/" && req.path !== "/weather") {
    return Response.json({ error: "not_found" }, { status: 404 });
  }

  const upstream = fetch("https://api.open-meteo.com/v1/forecast?latitude=52.52&longitude=13.41&current=temperature_2m,relative_humidity_2m,wind_speed_10m,is_day&timezone=auto", {
    headers: {
      "Accept": "application/json",
    },
    maxResponseBytes: 65536,
  });

  if (!upstream.ok) {
    return Response.json({
      error: "weather_unavailable",
      upstreamStatus: upstream.status,
    }, { status: 502 });
  }

  const forecast = upstream.json();
  return Response.json({
    app: "Weather Forecasts",
    source: "open-meteo",
    upstreamRequestId: upstream.headers.get("x-request-id") ?? "none",
    coordinates: {
      latitude: forecast.latitude,
      longitude: forecast.longitude,
    },
    timezone: forecast.timezone,
    current: {
      time: forecast.current.time,
      temperature: forecast.current.temperature_2m,
      temperatureUnit: forecast.current_units.temperature_2m,
      humidity: forecast.current.relative_humidity_2m,
      humidityUnit: forecast.current_units.relative_humidity_2m,
      windSpeed: forecast.current.wind_speed_10m,
      windSpeedUnit: forecast.current_units.wind_speed_10m,
      isDay: forecast.current.is_day,
    },
  });
}
