import sys
import backoff
import openai
from openai import OpenAI
from private_data import PrivateData
from datetime import datetime

instructions = """
You are an assistant in evaluating the private data contained in network requests from apps on an android phone.
You consider the following categories of data private data:
Model
OS
DeviceName
Language
TimeZone
UserAgent
Orientation
Carrier
Rooted
Emulator
Width
Height
Roaming
Uptime
RamTotal
RamFree
NetworkConnectionType
SignalStrengthCellular
SignalStrengthWifi
IsCharging
BatteryPercentage
BatteryState
DiskTotal
DiskFree
AccelerometerX
AccelerometerY
AccelerometerZ
RotationX
RotationY
RotationZ
MacAddress
Architecture
DarkMode
LocalIp
Volume
Country
Latitude
Longitude
PublicIP
SDKVersion
AppID
AppVersion
InForeground
CurrentlyViewed

You receive the query string or the payload of a network request as input.
These input could be encoded in different formats such as json, xml, url encoding or other data formats commonly used in requests.

For each key value pair of data that exists in the request you provide the key, value and one data category from above that best matches the category of the key value pair.
You format your output as comma separated values for the following header:
Data Category, Key, Value
You do not include the header line in the output.
You only provide output if you can identify the data categories and the corresponding key value pairs.
Otherwise your only output is: no-data
"""

gpt4 = "gpt-4-1106-preview"
gpt3 = "gpt-3.5-turbo"
gpt3_16k = "gpt-3.5-turbo-16k"


class LLM:
    def __init__(self,  model: str, source: str):
        self._client = OpenAI(
            timeout=10.0
        )
        self.model = model
        self.source = source
        self.header = PrivateData("Data Category", "Key", "Value", self.source, self.model)

    def __del__(self):
        self._client.close()

    @backoff.on_exception(backoff.expo, openai.RateLimitError, max_tries=3, max_time=30.0)
    def _completion_with_backoff(self, **kwargs):
        try:
            return self._client.with_options(timeout=10.0).chat.completions.create(**kwargs)
        except openai.BadRequestError as e:
            if e.code == 'context_length_exceeded' and self.model == gpt3:
                nargs = kwargs
                nargs["model"] = gpt3_16k
                return self._completion_with_backoff(**nargs)
                # print(e)
                # return None
            else:
                print(e)
                return None
        except openai.APITimeoutError as e:
            print(e)
            return None

    def get_private_data(self, prompt: str) -> list[PrivateData]:
        print(f"prompting with size: {len(prompt)} - {prompt[:50]}")
        response = self._completion_with_backoff(
            model=self.model,
            messages=[
                {"role": "system", "content": instructions},
                {"role": "user", "content": prompt}
            ],
            temperature=0.68,
            max_tokens=403,
            top_p=1,
            frequency_penalty=0,
            presence_penalty=0,
            stop=["no-data"]
        )
        if response is None:
            return []
        answer = response.choices[0].message.content
        print(answer)
        private_data: list[PrivateData] = []
        if answer is not None and answer != "":
            for line in answer.split("\n"):
                try:
                    category, key, value = line.split(",")
                    data = PrivateData(category.strip().replace(' ', ''), key.strip(), value.strip(), self.source, self.model)
                    if not data.same_value(self.header):
                        private_data.append(data)
                except ValueError as e:
                    print(f"{datetime.now()}: llm answer could not be parsed: {line}", file=sys.stderr)

        return private_data
