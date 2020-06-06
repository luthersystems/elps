package events

type EventType string
const EventTypeStarted = EventType("started")
const EventTypeContinued = EventType("continued")
const EventTypeExited = EventType("exited")
const EventTypeStoppedBreakpoint = EventType("stopped-breakpoint")
const EventTypeStoppedPaused = EventType("stopped-paused")
const EventTypeStoppedStep = EventType("stopped-step")
const EventTypeStoppedEntry = EventType("stopped-entry")
const EventTypeTerminated = EventType("terminated")
