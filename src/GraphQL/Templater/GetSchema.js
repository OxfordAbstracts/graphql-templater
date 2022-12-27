import "isomorphic-fetch";
import { getIntrospectionQuery, printSchema, buildClientSchema } from "graphql";

export function getGqlSchemaImpl({ url, token }) {
  return async () => {
    try {
      const introspectionQuery = getIntrospectionQuery();

      const response = await fetch(url, {
        method: "POST",
        headers: token
          ? {
              "Content-Type": "application/json",
              Authorization: `Bearer ${token}`,
            }
          : {
              "Content-Type": "application/json",
            },
        body: JSON.stringify({ query: introspectionQuery }),
      });

      const { data } = await response.json();

      // console.log("data", data)

      const schema = printSchema(buildClientSchema(data));

      // console.log({schema})

      return schema;
    } catch (err) {
      console.error("failed to get gql schema", err);
      throw err;
    }
  };
}
