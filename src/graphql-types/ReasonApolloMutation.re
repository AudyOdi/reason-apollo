open ReasonApolloTypes;

module MutationFactory = (Config: ReasonApolloTypes.Config) => {
  external cast :
    string =>
    {
      .
      "data": Js.Json.t,
      "loading": bool,
    } =
    "%identity";
  [@bs.module] external gql : ReasonApolloTypes.gql = "graphql-tag";
  [@bs.module]
  external mutationComponent : ReasonReact.reactClass = "react-apollo";
  let graphqlQueryAST = gql(. Config.query);
  type state =
    | NotCalled
    | Loading
    | Loaded(Js.Json.t)
    | Failed(Js.Promise.error);
  type response =
    | Loading
    | Error(apolloError)
    | Data(Config.t)
    | NoData;
  type renderPropObj = {data: response};
  type renderPropObjJS = {
    .
    "loading": Js.boolean,
    "data": Js.Nullable.t(Js.Json.t),
    "error": Js.Nullable.t(apolloError),
  };
  type mutationFuncParam = {. "variables": Js.Null_undefined.t(Js.Json.t)};
  type mutationFunc = mutationFuncParam => unit;
  let apolloDataToReason: renderPropObjJS => response =
    apolloData =>
      switch (
        apolloData##loading |> Js.to_bool,
        apolloData##data |> Js.Nullable.to_opt,
        apolloData##error |> Js.Nullable.to_opt,
      ) {
      | (true, _, _) => Loading
      | (false, Some(response), _) => Data(Config.parse(response))
      | (false, _, Some(error)) => Error(error)
      | (false, None, None) => NoData
      };
  let convertJsInputToReason = (apolloData: renderPropObjJS) => {
    data: apolloDataToReason(apolloData),
  };
  let make =
      (children: (mutationFunc, renderPropObj) => ReasonReact.reactElement) =>
    ReasonReact.wrapJsForReason(
      ~reactClass=mutationComponent,
      ~props=Js.Nullable.({"mutation": graphqlQueryAST}),
      (mutation, apolloData) => {
        let data = apolloData |> convertJsInputToReason;
        children(mutation, data);
      },
    );
};