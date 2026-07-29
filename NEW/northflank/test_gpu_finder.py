from gpu_finder import managed_candidates, service_payload


def regions():
    return {
        "regions": [
            {
                "id": "z-region",
                "gpuDevices": [
                    {
                        "id": "h100-80",
                        "countOptions": [1, 8],
                        "pricing": {"onDemand": 274},
                    },
                    {"id": "a100-80", "countOptions": [8]},
                ],
            },
            {
                "id": "a-region",
                "gpuDevices": [
                    {
                        "id": "h200-141",
                        "countOptions": [8],
                        "pricing": {"onDemand": 314},
                    }
                ],
            },
        ]
    }


def test_inventory_only_returns_requested_hopper_gpus():
    values = managed_candidates(regions(), count=8, project_prefix="race")
    assert [(value.region, value.gpu_type) for value in values] == [
        ("a-region", "h200-141"),
        ("z-region", "h100-80"),
    ]


def test_service_has_exactly_eight_requested_gpus():
    candidate = managed_candidates(regions(), count=8, project_prefix="race")[0]
    payload = service_payload(candidate)
    assert payload["deployment"]["gpu"]["configuration"] == {
        "gpuType": "h200-141",
        "gpuCount": 8,
    }
    assert payload["billing"]["deploymentPlan"] == "nf-gpu-h200-141-8g"


def test_region_filter_is_exact():
    values = managed_candidates(
        regions(),
        count=8,
        project_prefix="race",
        allowed_regions={"z-region"},
    )
    assert [value.region for value in values] == ["z-region"]
