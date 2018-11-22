# Design of AWS-based P&T deployment

## API Lambda

The core APIs are exposed with a Lambda fronted by API Gateway. All of the API
endpoints like `POST /`, `GET /movement_options/...`, etc, are exposed in this
way, and (probably?) all served by the same lambda function.

The one exception to this is the state-change polling, which is being replaced.

## Polling w/ AppSync & GraphQL

Instead of having a `/poll` endpoint, we will make use of AWS AppSync. This is
the cheapest way I've found to do real-time notifications on AWS infrastructure.

We'll expose a single GraphQL subcription, "newGameState", which is triggered by
the Lambda function when any game commands are run. Because of the way AppSync
is implemented, the only way we can trigger this subscription is by running a
GraphQL mutation, so the Lambda function will need to run a GraphQL client as
well.

**TBD** How do we scope the notification & subscription event to a specific
game, so only the relevant subscribers get notified?

## Authentication & Authorization

AppSync has a few different ways that it can do authentication, but you have to
choose a *single* authentication mechanism.

- fixed key
- cognito
- AWS IAM

Because we want our end-users to connect to AppSync from their browsers, the
Cognito-based authentication is the only option for us. Unfortunately, because
AppSync only allows one authentication mechanism, that means our Lambda
function, which wants to run the GraphQL mutation in order to notify clients,
will *also* need to authenticate with Cognito.

So we have a Cognito user pool set up with a "pandt-backend" user in a special
"backend" group, which we can then use as an authorization predicate in order to
trigger the notification Mutation. All other users will not be in any groups and
will only be able to subscribe to these notifications.
