# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Install dependencies
npm install

# Run the example application
npm run dev

# Type check without building
npm run typecheck

# Build TypeScript to JavaScript
npm run build
```

## Architecture Overview

This project demonstrates functional domain modeling in TypeScript using fp-ts. Key architectural patterns:

### Type System
- **Branded Types**: Primitive types are wrapped with semantic meaning using `Brand<K, T>` to prevent accidental type confusion
- **Smart Constructors**: All value objects use validation-enforced factory functions returning `Either<Error, Value>`
- **Algebraic Data Types**: Order status uses tagged unions with state-specific data, preventing invalid states at compile time

### Functional Patterns
- **Either Monad**: Error handling as values using fp-ts's Either type, enabling composable error propagation
- **Pure Functions**: All operations are side-effect free, returning new immutable values
- **Function Composition**: Heavy use of `pipe` and `E.chain` for sequential operation chaining

### Domain Modeling
- **Value Objects**: Complex concepts like Money combine data with invariants
- **Domain Constraints**: Business rules encoded in types (e.g., PositiveNumber) and operations
- **Make Illegal States Unrepresentable**: Type system prevents invalid states rather than runtime validation

### Code Structure
- `src/domain/types.ts`: Core domain types, branded types, and value object constructors
- `src/domain/operations.ts`: Pure business logic functions operating on domain types
- `src/index.ts`: Example usage demonstrating the functional domain model in action

When modifying this codebase, maintain the functional programming principles: immutability, pure functions, and type-driven development using fp-ts utilities.

## Code Guidelines

- Always remove unused imports when modifying code