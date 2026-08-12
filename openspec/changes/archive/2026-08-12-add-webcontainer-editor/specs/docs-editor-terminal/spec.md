## Purpose

Provides a dedicated docs route with an interactive browser terminal whose input, output, resizing, failures, and cleanup follow the shared editor execution environment.

## ADDED Requirements

### Requirement: Dedicated editor route
The docs application SHALL serve an editor application at `/editor` whose initial usable surface is an interactive terminal and whose layout can later host additional editor subsystems without replacing the execution environment.

#### Scenario: User opens the editor
- **WHEN** a supported browser navigates to `/editor`
- **THEN** the page presents a terminal surface and begins acquiring the shared editor environment

#### Scenario: Editor is server rendered
- **WHEN** the `/editor` route is rendered before browser hydration
- **THEN** the route renders a stable initial state without attempting to boot WebContainer on the server

### Requirement: Interactive shell session
The editor terminal SHALL run one scoped interactive `jsh` process and SHALL preserve WebContainer's combined terminal-output semantics. Terminal input SHALL reach the shell in order, and every output chunk SHALL be rendered in emission order by one terminal-output consumer.

#### Scenario: User enters a command
- **WHEN** the user types a command and submits it in the terminal
- **THEN** the shell receives the corresponding input in order and the terminal renders the resulting combined output in order

#### Scenario: Shell writes multiple output chunks
- **WHEN** the shell emits multiple terminal-output chunks
- **THEN** the terminal renders every chunk exactly once and in the order delivered by WebContainer

### Requirement: Terminal dimensions follow the visible surface
The editor terminal SHALL fit its visible container and SHALL propagate positive column and row changes to the running shell process.

#### Scenario: Terminal container resizes
- **WHEN** the editor layout changes the terminal container dimensions
- **THEN** the terminal refits and the shell receives the resulting positive column and row dimensions

### Requirement: Terminal lifecycle is resource safe
The shell, its input pipeline, output subscription, resize observation, and terminal renderer SHALL be scoped to the mounted terminal surface. Closing that scope SHALL interrupt active pipelines and release the running process without manual teardown exposed to components.

#### Scenario: User leaves the editor with a running shell
- **WHEN** the terminal unmounts while `jsh` is still running
- **THEN** input and output processing stop, the process is terminated once, and the terminal renderer is disposed

#### Scenario: React remounts the terminal during development
- **WHEN** the terminal is mounted, immediately unmounted, and remounted by development lifecycle checks
- **THEN** the application does not retain an abandoned process, duplicate output consumer, or competing WebContainer boot

### Requirement: Terminal state communicates waiting and failure
The editor SHALL render terminal session initialization as an exhaustive waiting, success, or failure state. A typed WebContainer boot or process failure SHALL remain visible and SHALL not produce an unhandled promise rejection or a permanently blank terminal.

#### Scenario: WebContainer boot is pending
- **WHEN** the editor environment has not finished booting
- **THEN** the terminal surface communicates that initialization is in progress

#### Scenario: Browser cannot boot WebContainer
- **WHEN** WebContainer boot fails because the browser or hosting environment is unsupported or misconfigured
- **THEN** the editor renders an actionable failure state derived from the typed failure

### Requirement: Editor route is cross-origin isolated
Responses for the `/editor` document SHALL set the cross-origin opener and embedder policies required by the configured WebContainer boot mode in both development and production hosting.

#### Scenario: Browser loads the editor document
- **WHEN** the browser requests `/editor`
- **THEN** the response enables the cross-origin isolation mode expected by the editor's WebContainer runtime
