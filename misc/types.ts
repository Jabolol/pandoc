type JString = string;
type JNumber = number;
type JBoolean = boolean;
type JNull = null;
type JObject = { [key: string]: JValue };
type JValue = JString | JNumber | JBoolean | JNull | JObject | JArray;
type JArray = JValue[];

type JNode = {
  key: string;
  value: JValue;
};

type JDocument = {
  header: JNode[];
  body: JNode[];
};
